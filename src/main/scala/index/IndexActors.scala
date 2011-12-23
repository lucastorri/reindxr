package co.torri.reindxr.index

import java.io._
import akka.actor._


case class SearchIndexResult(query: String, files: List[File])
case class SearchIndex(query: String)
case class InsertIndex(file: File)
case class RemoveIndex(file: File)

case class FilesIndexerActor(index: FilesIndex) extends Actor {
	
	def receive = {
		case InsertIndex(file) => index.insert(file)
		case RemoveIndex(file) => index.remove(file)
	}
	
}

case class IndexSearcherActor(index: FilesIndex) extends Actor {
	
	def receive = {
		case SearchIndex(query) =>
			self.reply(SearchIndexResult(query, index.search(query)))
	}

}