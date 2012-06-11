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

case class DocMatchRep(id: String, matches: Seq[String])
object DocMatchRep {
	def apply(d: DocMatch) : DocMatchRep = apply(d.doc.id, d.matches)
}

case class HttpServer(index: DocIndex, port: Int) {
  
	val handler = async.Planify {
		case req @ GET(Path(Seg("search" :: query :: Nil))) =>
		  	req.respond(JsonContent ~> ResponseString(index.search(query).map(DocMatchRep(_)).toJson.toString))
		case req @ GET(Path(Seg("hl" :: id :: query :: Nil))) =>
		  	req.respond(JsonContent ~> ResponseString(new DocMatchRep(id, List(index.highlight(query, id))).toJson.toString))
		case req =>
		    req.respond(NotFound ~> ResponseString("not found"))
	}
  
    private val s = Http(port).chunked(1048576).plan(handler)

    def start : Unit = s.start
    
    def stop : Unit = s.stop
}