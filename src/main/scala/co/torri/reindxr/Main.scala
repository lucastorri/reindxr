package co.torri.reindxr

import java.io.File

import co.torri.reindxr.http._
import co.torri.reindxr.users.UsersMonitor
import com.typesafe.scalalogging.LazyLogging


object Main extends LazyLogging {

  def main(args: Array[String]): Unit = {

    val dataDir = new File(args(0))
    val indexDir = new File(args(1))
    val serverPort = args.lift(2).map(_.toInt).getOrElse(8123)

    logger.info("Creating monitor")
    val monitor = UsersMonitor(dataDir, indexDir)
    logger.info("Creating server")
    val server = HttpServer(monitor, serverPort)

    logger.info("Adding shutdown hook")
    Runtime.getRuntime.addShutdownHook(new Thread {
      override def run(): Unit = {
        server.stop()
        monitor.close()
      }
    })

    logger.info("Starting components")
    monitor.start()
    server.start()
  }

}
