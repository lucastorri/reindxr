package co.torri.reindxr.http

import cats.effect.{ConcurrentEffect, ContextShift, IO, Timer}
import co.torri.reindxr.index.{DocIndex, DocIndexes, DocMatch}
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

case class MatchedResponse(id: String, matches: Seq[String], metadata: Map[String, String])

object MatchedResponse {
  def apply(d: DocMatch): MatchedResponse =
    MatchedResponse(d.doc.id, d.matches, d.doc.metadata)
}

case class HttpServer(indexes: DocIndexes, port: Int) extends LazyLogging {

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(global)
  private implicit val concurrentEffect: ConcurrentEffect[IO] = IO.ioConcurrentEffect(contextShift)
  private implicit val timer: Timer[IO] = IO.timer(global)

  private lazy val userNotFound: IO[Response[IO]] =
    Forbidden(Ser("user not found"))

  private val service: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / username / "search" / query =>
      json(req, username) { index =>
        Ser(index.search(query))
      }

    case req@GET -> Root / username / "snippets" / query =>
      json(req, username) { index =>
        Ser(index.snippets(query))
      }

    case req@GET -> Root / username / "snippets" / documentId / query =>
      json(req, username) { index =>
        Ser(index.snippets(documentId, query))
      }

    case req@GET -> Root / username / "hl" / documentId / query =>
      json(req, username) { index =>
        Ser(index.highlight(documentId, query))
      }

    case _ =>
      NotFound(Ser("not found"))
  }

  private def json(req: Request[IO], username: String)(f: DocIndex => Json): IO[Response[IO]] =
    try {
      indexes.index(username).map(docIndex => Ok(f(docIndex))).getOrElse(userNotFound)
    } catch {
      case e: Exception =>
        logger.error("Error", e)
        InternalServerError(Ser(e.getMessage))
    }

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

object Ser {
  private def create(s: Seq[MatchedResponse]): Json =
    s.asJson

  def apply(m: MatchedResponse): Json =
    create(List(m))

  def apply(m: DocMatch): Json =
    apply(MatchedResponse(m))

  def apply(s: Seq[DocMatch]): Json =
    create(s.map(MatchedResponse(_)))

  def apply(error: String): Json =
    json"""{"error": $error}"""
}

object Decode {

  import java.net.URLDecoder
  import java.nio.charset.Charset

  trait Decoder {
    def charset: Charset

    def unapply(encoded: String): Option[String] = try {
      Some(URLDecoder.decode(encoded, charset.name))
    } catch {
      case _: Exception => None
    }
  }

  object utf8 extends Decoder {
    val charset: Charset = Charset.forName("utf8")
  }

}