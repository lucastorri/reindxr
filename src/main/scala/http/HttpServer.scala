package co.torri.reindxr.http

import co.torri.jsonr.any2json
import co.torri.reindxr.index.DocIndex
import unfiltered.netty.Http
import unfiltered.netty.async
import unfiltered.request.GET
import unfiltered.request.Path
import unfiltered.request.Seg
import unfiltered.response.JsonContent
import unfiltered.response.PlainTextContent
import unfiltered.response.ResponseString
import unfiltered.request._
import unfiltered.response._
import org.jboss.netty.handler.codec.http.HttpResponse
import org.apache.lucene.document.Document
import co.torri.reindxr.index.DocMatch

trait Response
case class MatchedResponse(id: String, matches: Seq[String], metadata: Map[String, String]) extends Response
object Response {
	def apply(d: DocMatch) : Response = 
		MatchedResponse(d.doc.id, d.matches, d.doc.metadata)
}

case class HttpServer(index: DocIndex, port: Int) {
  
	import Decode.{utf8 => dec}
  
	val handler = async.Planify {
	  	case req @ GET(Path(Seg("search" :: dec(query) :: Nil))) =>
		  	req.respond(Json(index.search(query)))
		  	
		case req @ GET(Path(Seg("snippets" :: dec(query) :: Nil))) =>
		  	req.respond(Json(index.snippets(query)))
		  	
		case req @ GET(Path(Seg("snippets" :: dec(id) :: dec(query) :: Nil))) =>
		  	req.respond(Json(index.snippets(id, query)))
		  	
		case req @ GET(Path(Seg("hl" :: dec(id) :: dec(query) :: Nil))) =>
		  	req.respond(Json(index.highlight(id, query)))
		  	
		case req =>
		    req.respond(NotFound ~> ResponseString("not found"))
	}
	
	object Json {
		private def create(s: Seq[Response]) : ResponseFunction[HttpResponse] = 
			JsonContent ~> ResponseString(s.toJson.toString)
		def apply(m: Response) : ResponseFunction[HttpResponse] =
		  	create(List(m))
		def apply(m: DocMatch) : ResponseFunction[HttpResponse] =
		  	apply(Response(m))
		def apply(s: Seq[DocMatch]) : ResponseFunction[HttpResponse] =
		  	create(s.map(Response(_)))
	}
  
    private val s = Http(port).chunked(1048576).plan(handler)

    def start : Unit = s.start
    
    def stop : Unit = s.stop
}

object Decode {
	import java.net.URLDecoder
	import java.nio.charset.Charset

	trait Decoder {
		def charset: Charset
		def unapply(encoded: String) = try {
			Some(URLDecoder.decode(encoded, charset.name))
		} catch { case _ => None }
	}

	object utf8 extends Decoder {
		val charset = Charset.forName("utf8")
	}
}