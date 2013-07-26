package co.torri.reindxr

import java.io.File

import co.torri.reindxr.filemon._
import co.torri.reindxr.http._
import co.torri.reindxr.index._
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
        
        implicit def dataFile2doc(f: DataFile) = Doc(dataFolder, f)
        val eventHandler : FileEvent => Unit = {
          	case FileCreated(df) => index.insert(df)
            case FileModified(df) => index.insert(df)
            case FileDeleted(df) => index.remove(df)
        }
        
        logger.info("Creating monitor")
        val monitor = FileMonitor(dataFolder, eventHandler)
        logger.info("Creating server")
        val server = HttpServer(index, serverPort)
        
        logger.info("Adding shutdown hook")
        Runtime.getRuntime.addShutdownHook(new Thread {
            override def run = {
            	server.stop
                monitor.close
                index.close
            }
        })

        logger.info("Starting components")
        server.start
        monitor.start
    }

}
