package co.torri.reindxr.http

import cats.effect.{ConcurrentEffect, ContextShift, IO, Timer}
import co.torri.reindxr.take2.{AccountId, DocumentId, DocumentMatch, Reindxr}
import com.typesafe.scalalogging.LazyLogging
import io.circe._
import io.circe.generic.auto._
import io.circe.literal._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext.global

case class MatchedResponse(id: String, matches: Seq[String])

object MatchedResponse {
  def apply(documentMatch: DocumentMatch): MatchedResponse =
    MatchedResponse(documentMatch.documentId.value, documentMatch.fragments)
}

case class HttpServer(accountReindxr: AccountId => IO[Option[Reindxr]], port: Int) extends LazyLogging {

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(global)
  private implicit val concurrentEffect: ConcurrentEffect[IO] = IO.ioConcurrentEffect(contextShift)
  private implicit val timer: Timer[IO] = IO.timer(global)

  private val userNotFound: IO[Response[IO]] =
    Forbidden(JsonEncoder.error("User not found"))

  private val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / accountId / "search" / query =>
      withReindxr(req, accountId) { reindxr =>
        reindxr.search(query)
      }

    case req@GET -> Root / accountId / "snippets" / query =>
      withReindxr(req, accountId) { reindxr =>
        reindxr.snippets(query)
      }

    case req@GET -> Root / accountId / "snippets" / documentId / query =>
      withReindxr(req, accountId) { reindxr =>
        reindxr.snippets(DocumentId(documentId), query)
      }

    case req@GET -> Root / accountId / "hl" / documentId / query =>
      withReindxr(req, accountId) { reindxr =>
        reindxr.highlight(DocumentId(documentId), query)
      }

    case _ =>
      NotFound(JsonEncoder.error("Not found"))
  }

  private lazy val server =
    BlazeServerBuilder[IO](global)
      .bindHttp(port, "localhost")
      .withHttpApp(Router("/" -> service).orNotFound)
      .resource
      .use(_ => IO.never)
      .start
      .unsafeRunSync()

  private def withReindxr[T: JsonEncoder](req: Request[IO], accountId: String)(f: Reindxr => IO[T]): IO[Response[IO]] =
    accountReindxr(AccountId(accountId))
      .flatMap {
        case Some(reindxr) => Ok(f(reindxr).map(implicitly[JsonEncoder[T]].encode))
        case None => userNotFound
      }
      .handleErrorWith(e => InternalServerError(JsonEncoder.error(e.getMessage)))

  def start(): Unit =
    server

  def stop(): Unit =
    server.cancel.unsafeRunSync()

}

trait JsonEncoder[T] {
  def encode(content: T): Json
}

object JsonEncoder {

  implicit val matchedResponseEncoder: JsonEncoder[MatchedResponse] =
    mr => List(mr).asJson
  implicit val documentMatchEncoder: JsonEncoder[DocumentMatch] =
    dm => matchedResponseEncoder.encode(MatchedResponse(dm))
  implicit val documentMatchSeqEncoder: JsonEncoder[Seq[DocumentMatch]] = seq =>
    seq.map(MatchedResponse(_)).asJson
  implicit val optionalDocumentMatchEncoder: JsonEncoder[Option[DocumentMatch]] = dmOpt =>
    documentMatchSeqEncoder.encode(dmOpt.toSeq)

  def error(error: String): Json =
    json"""{"error": $error}"""
}
