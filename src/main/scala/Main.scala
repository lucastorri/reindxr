package co.torri.reindxr

import java.io.File

import akka.actor.Actor.actorOf
import akka.actor.Actor.registry
import akka.actor.Actor.remote
import akka.actor.actorRef2Scala
import filemon.FileCreated
import filemon.FileDeleted
import filemon.FileEvent
import filemon.FileModified
import filemon.FileMonitor
import index.FilesIndex.indexFrom
import index.FilesIndexerActor
import index.IndexSearcherActor
import index.InsertIndex
import index.RemoveIndex


object Main {

	def main(args: Array[String]) : Unit = {
		
		val dataFolder = new File(args(0))
		val indexFolder = new File(args(1))
		val serverAddress = args.lift(2).getOrElse("localhost")
		val serverPort = args.lift(3).map(_.toInt).getOrElse(8123)
		
		val index = indexFrom(indexFolder)

		val indexer = actorOf(FilesIndexerActor(index)).start
		val dataEventHandler: PartialFunction[FileEvent, Unit] = {
			case FileCreated(file) => indexer ! InsertIndex(file)
			case FileModified(file) => indexer ! InsertIndex(file)
			case FileDeleted(file) => indexer ! RemoveIndex(file)
		}
		
	    remote
	    	.start(serverAddress, serverPort)
	    	.register("search-service", actorOf(IndexSearcherActor(index)))
		
		val monitor = FileMonitor(dataFolder, dataEventHandler)
		
		Runtime.getRuntime.addShutdownHook(new Thread {
            override def run = {
                monitor.stop
                registry.shutdownAll
            }
        })
		
		monitor.start
	}
	
}