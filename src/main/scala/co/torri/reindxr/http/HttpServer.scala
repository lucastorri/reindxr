package co.torri.reindxr.http

import co.torri.reindxr.index.{DocIndexes, DocIndex}
import unfiltered.netty._
import unfiltered.request._
import unfiltered.response._
import org.jboss.netty.handler.codec.http.HttpResponse
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write
import com.typesafe.scalalogging.slf4j.Logging
import scala.Some
import co.torri.reindxr.index.DocMatch
import unfiltered.Async
import unfiltered.response.ResponseString

trait Response
case class MatchedResponse(id: String, matches: Seq[String], metadata: Map[String, String]) extends Response
object Response {
	def apply(d: DocMatch) : Response = 
		MatchedResponse(d.doc.id, d.matches, d.doc.metadata)
}

case class HttpServer(indexes: DocIndexes, port: Int) extends Logging {
  
	import Decode.{utf8 => dec}
  private type Req = HttpRequest[ReceivedMessage] with Async.Responder[HttpResponse]
  
	object handler extends async.Plan with ServerErrorResponse {
    def intent = {
      case req @ GET(Path(Seg(user :: "search" :: dec(query) :: Nil))) =>
        json(req, user) {
          index => Json(index.search(query))
        }

      case req @ GET(Path(Seg(user :: "snippets" :: dec(query) :: Nil))) =>
        json(req, user) {
          index => Json(index.snippets(query))
        }

      case req @ GET(Path(Seg(user :: "snippets" :: dec(id) :: dec(query) :: Nil))) =>
        json(req, user) {
          index => Json(index.snippets(id, query))
        }

      case req @ GET(Path(Seg(user :: "hl" :: dec(id) :: dec(query) :: Nil))) =>
        json(req, user) {
          index => Json(index.highlight(id, query))
        }

      case req =>
        req.respond(NotFound ~> Json("error" -> "not found"))
    }
  }

  private val userNotFound =
    Unauthorized ~> Json("error" -> "user not found")

  private def json(req: Req, username: String)(f: DocIndex => ResponseFunction[HttpResponse]) : Unit =
    try req.respond {
      indexes.index(username).map(f).getOrElse(userNotFound)
    } catch {
      case e: Exception =>
        logger.error("Error", e)
        InternalServerError ~> Json("error" -> e.getMessage)
    }
	
  private val server =
    Http(port).chunked(1048576).plan(handler)

  def start() : Unit =
    server.start

  def stop() : Unit =
    server.stop

}

object Json {
  implicit val formats = Serialization.formats(NoTypeHints)

  private def create(s: Seq[Response]) : ResponseFunction[HttpResponse] =
    JsonContent ~> ResponseString(write(s))

  def apply(m: Response) : ResponseFunction[HttpResponse] =
    create(List(m))

  def apply(m: DocMatch) : ResponseFunction[HttpResponse] =
    apply(Response(m))

  def apply(s: Seq[DocMatch]) : ResponseFunction[HttpResponse] =
    create(s.map(Response(_)))

  def apply(error: (String, String)) =
    JsonContent ~> ResponseString(write(Map(error)))
}

object Decode {
	import java.net.URLDecoder
	import java.nio.charset.Charset

	trait Decoder {
		def charset: Charset
		def unapply(encoded: String) = try {
			Some(URLDecoder.decode(encoded, charset.name))
		} catch { case _: Exception => None }
	}

	object utf8 extends Decoder {
		val charset = Charset.forName("utf8")
	}
}