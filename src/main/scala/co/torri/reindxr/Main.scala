package co.torri.reindxr

import java.io.File

import co.torri.reindxr.http._
import users.UsersMonitor
import com.typesafe.scalalogging.slf4j.Logging


object Main extends Logging {

  def main(args: Array[String]) : Unit = {

    val dataDir = new File(args(0))
    val indexDir = new File(args(1))
    val serverPort = args.lift(2).map(_.toInt).getOrElse(8123)

    logger.info("Creating monitor")
    val monitor = UsersMonitor(dataDir, indexDir)
    logger.info("Creating server")
    val server = HttpServer(null, serverPort) //TODO pass indexes

    logger.info("Adding shutdown hook")
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run = {
        server.stop
        monitor.close
      }
    })

    logger.info("Starting components")
    server.start
    monitor.start
  }

}
