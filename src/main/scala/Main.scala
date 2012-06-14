package co.torri.reindxr

import java.io.File

import co.torri.reindxr.filemon.FileCreated
import co.torri.reindxr.filemon.FileDeleted
import co.torri.reindxr.filemon.FileEvent
import co.torri.reindxr.filemon.FileModified
import co.torri.reindxr.filemon.FileMonitor
import co.torri.reindxr.http.HttpServer
import co.torri.reindxr.index.Doc
import co.torri.reindxr.index.DocIndex
import grizzled.slf4j.Logger


object Main {

    private def logger = Logger("reindxr")

    def main(args: Array[String]) : Unit = {

        val dataFolder = new File(args(0))
        val indexFolder = new File(args(1))
        val serverPort = args.lift(2).map(_.toInt).getOrElse(8123)
        
        logger.info("Reindxr starting")
        
        logger.info("Creating index")
        val index = DocIndex(indexFolder, dataFolder)
        
        implicit def file2doc(f: File) = Doc(dataFolder, f)
        val eventHandler : PartialFunction[FileEvent, Unit] = {
          	case FileCreated(file) => index.insert(file)
            case FileModified(file) => index.insert(file)
            case FileDeleted(file) => index.remove(file)
        }
        
        logger.info("Creating monitor")
        val monitor = FileMonitor(dataFolder, eventHandler)
        logger.info("Creating server")
        val server = HttpServer(index, serverPort)
        
        logger.info("Adding shutdown hook")
        Runtime.getRuntime.addShutdownHook(new Thread {
            override def run = {
            	server.stop
                monitor.stop
                index.close
            }
        })

        logger.info("Starting components")
        server.start
        monitor.start
    }

}
