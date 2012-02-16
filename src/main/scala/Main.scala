package co.torri.reindxr

import java.io.File

import akka.actor.Props
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
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

    val system = ActorSystem("ReindxrApplication", ConfigFactory.load.getConfig("search"))

    val indexer = system.actorOf(Props(FilesIndexerActor(index, dataFolder)), "indexer")
    val dataEventHandler: PartialFunction[FileEvent, Unit] = {
      case FileCreated(file) => indexer ! InsertIndex(file)
      case FileModified(file) => indexer ! InsertIndex(file)
      case FileDeleted(file) => indexer ! RemoveIndex(file)
    }

    system.actorOf(Props(IndexSearcherActor(index)), "search-service")

    val monitor = FileMonitor(dataFolder, dataEventHandler)

    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run = {
        monitor.stop
        system.shutdown
        index.close
      }
    })

    monitor.start
  }

}
