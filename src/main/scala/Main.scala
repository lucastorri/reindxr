package co.torri.reindxr

import filemon._
import index._
import FilesIndex.indexFrom

import akka.actor._
import Actor._
import java.io.File


object Main {

	def main(args: Array[String]) : Unit = {
		
		val dataFolder = new File(args(0))
		val indexFolder = new File(args(1))
		val index = indexFrom(indexFolder)

		val indexer = actorOf(FilesIndexerActor(index)).start
		val dataEventHandler: PartialFunction[FileEvent, Unit] = {
			case FileCreated(file) => indexer ! InsertIndex(file)
			case FileModified(file) => indexer ! InsertIndex(file)
			case FileDeleted(file) => indexer ! RemoveIndex(file)
		}
		
	    remote.start("localhost", 8123)
		remote.register("search-service", actorOf(IndexSearcherActor(index)))
		
		FileMonitor(dataFolder, dataEventHandler).start
	}
	
}