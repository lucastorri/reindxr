package co.torri.reindxr.index

import java.io.File

import akka.actor.Actor


case class InsertIndex(file: File)
case class RemoveIndex(file: File)

case class SearchIndexResult(query: String, files: List[(String, List[String])])
case class SearchIndex(query: String)
case class HighlightResult(query: String, file: String)

case class FilesIndexerActor(index: FilesIndex, basepath: String) extends Actor {
	
	def receive = {
		case InsertIndex(file) => index.insert(FileDoc(basepath, file))
		case RemoveIndex(file) => index.remove(FileDoc(basepath, file))
	}
	
}
object FilesIndexerActor {
	def apply(index: FilesIndex, basepath: File) : FilesIndexerActor = apply(index, basepath.getAbsolutePath)
}

case class IndexSearcherActor(index: FilesIndex) extends Actor {
	
	def receive = {
		case SearchIndex(query) =>
			self.reply(SearchIndexResult(query, index.search(query)))
		case HighlightResult(query, file) =>
			println(query + " }> " + file)
			self.reply(index.highlight(query, file))
	}

}