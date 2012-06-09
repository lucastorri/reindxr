package co.torri.reindxr

import java.io.File
import co.torri.reindxr.http._
import filemon.FileCreated
import filemon.FileDeleted
import filemon.FileEvent
import filemon.FileModified
import filemon.FileMonitor
import http.HttpServer
import index.DocIndex
import index.Doc


object Main {

    def main(args: Array[String]) : Unit = {

        val dataFolder = new File(args(0))
        val indexFolder = new File(args(1))
        val serverAddress = args.lift(2).getOrElse("localhost")
        val serverPort = args.lift(3).map(_.toInt).getOrElse(8123)
        
        
        val index = DocIndex(indexFolder, dataFolder)
        
        implicit def file2doc(f: File) = Doc(dataFolder, f)
        val eventHandler : PartialFunction[FileEvent, Unit] = {
          	case FileCreated(file) => index.insert(file)
            case FileModified(file) => index.insert(file)
            case FileDeleted(file) => index.remove(file)
        }
        
        val monitor = FileMonitor(dataFolder, eventHandler)
        val server = HttpServer(index, serverPort, serverAddress)
        
        Runtime.getRuntime.addShutdownHook(new Thread {
            override def run = {
            	server.stop
                monitor.stop
                index.close
            }
        })

        server.start
        monitor.start
    }

}
