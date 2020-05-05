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

case class HttpServer(reindxr: AccountId => IO[Option[Reindxr]], port: Int) extends LazyLogging {

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(global)
  private implicit val concurrentEffect: ConcurrentEffect[IO] = IO.ioConcurrentEffect(contextShift)
  private implicit val timer: Timer[IO] = IO.timer(global)

  private lazy val userNotFound: IO[Response[IO]] =
    Forbidden(JsonResponses.error("user not found"))

  private val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / username / "search" / query =>
      json(req, username) { index =>
        index.search(query).map(JsonResponses.encode)
      }

    case req@GET -> Root / username / "snippets" / query =>
      json(req, username) { index =>
        index.snippets(query).map(JsonResponses.encode)
      }

    case req@GET -> Root / username / "snippets" / documentId / query =>
      json(req, username) { index =>
        index.snippets(DocumentId(documentId), query).map(JsonResponses.encode)
      }

    case req@GET -> Root / username / "hl" / documentId / query =>
      json(req, username) { index =>
        index.highlight(DocumentId(documentId), query).map(JsonResponses.encode)
      }

    case _ =>
      NotFound(JsonResponses.error("not found"))
  }

  private def json(req: Request[IO], username: String)(f: Reindxr => IO[Json]): IO[Response[IO]] =
    reindxr(AccountId(username))
      .flatMap {
        case Some(index) => Ok(f(index))
        case None => userNotFound
      }
      .handleErrorWith(e => InternalServerError(JsonResponses.error(e.getMessage)))

  private lazy val server =
    BlazeServerBuilder[IO](global)
      .bindHttp(port, "localhost")
      .withHttpApp(Router("/" -> service).orNotFound)
      .resource
      .use(_ => IO.never)
      .start
      .unsafeRunSync()

  def start(): Unit =
    server

  def stop(): Unit =
    server.cancel.unsafeRunSync()

}

object JsonResponses {

  def encode(m: MatchedResponse): Json =
    List(m).asJson

  def encode(m: DocumentMatch): Json =
    encode(MatchedResponse(m))

  def encode(s: Seq[DocumentMatch]): Json =
    s.map(MatchedResponse(_)).asJson

  def error(error: String): Json =
    json"""{"error": $error}"""
}
