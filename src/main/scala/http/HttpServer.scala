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

case class DocMatchRep(id: String, matches: Seq[String] = List())
object DocMatchRep {
	def apply(d: DocMatch) : DocMatchRep = apply(d.doc.id, d.matches)
}

case class HttpServer(index: DocIndex, port: Int) {
  
	import Decode.{utf8 => dec}
  
	val handler = async.Planify {
	  	case req @ GET(Path(Seg("search" :: dec(query) :: Nil))) =>
		  	req.respond(JsonContent ~> ResponseString(index.search(query).map(DocMatchRep(_)).toJson.toString))
		case req @ GET(Path(Seg("snippets" :: dec(query) :: Nil))) =>
		  	req.respond(JsonContent ~> ResponseString(index.snippets(query).map(DocMatchRep(_)).toJson.toString))
		case req @ GET(Path(Seg("snippets" :: dec(id) :: dec(query) :: Nil))) =>
		  	req.respond(JsonContent ~> ResponseString("wip"))
		case req @ GET(Path(Seg("hl" :: dec(id) :: dec(query) :: Nil))) =>
		  	req.respond(JsonContent ~> ResponseString(DocMatchRep(id, List(index.highlight(query, id))).toJson.toString))
		case req =>
		    req.respond(NotFound ~> ResponseString("not found"))
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