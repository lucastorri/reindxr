package co.torri.reindxr.filemon

import java.io.File
import java.nio.file.FileVisitResult.CONTINUE
import java.nio.file.StandardWatchEventKinds._
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import com.typesafe.scalalogging.LazyLogging

import scala.jdk.CollectionConverters._


case class FileMonitor(dir: Path, handler: FileEvent => Unit) extends AutoCloseable with LazyLogging {

  private var started = false
  private val watcher = dir.getFileSystem.newWatchService()

  private def watch(path: Path): Unit = {
    def w(path: Path): WatchKey = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW)

    if (path.toFile.isDirectory) {
      w(path)
      Files.walkFileTree(path, new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attributes: BasicFileAttributes): FileVisitResult = {
          w(dir)
          CONTINUE
        }

        override def visitFile(file: Path, attributes: BasicFileAttributes): FileVisitResult = {
          logger.debug(s"File exists [$dir]: $file")
          handle(FileCreated(DataFile(file.toFile)))
          CONTINUE
        }
      })
    }
  }

  private val thread = new Thread {

    override def run(): Unit = {
      try {
        while (!Thread.currentThread().isInterrupted) {
          val key = watcher.take
          key.pollEvents.asScala.foreach { e =>
            val relativePath = e.context().asInstanceOf[Path]
            val path = key.watchable().asInstanceOf[Path].resolve(relativePath)
            val file = DataFile(path.toFile)
            val isFile = path.toFile.isFile
            e.kind match {
              case ENTRY_CREATE =>
                watch(path)
                if (isFile) {
                  logger.debug(s"File created [$dir]: $file")
                  handle(FileCreated(file))
                }
              case ENTRY_MODIFY =>
                if (isFile) {
                  logger.debug(s"File modified [$dir]: $file")
                  handle(FileModified(file))
                }
              case ENTRY_DELETE =>
                if (isFile) {
                  logger.debug(s"File deleted [$dir]: $file")
                  handle(FileDeleted(file))
                }
              case OVERFLOW =>
                logger.error(s"Overflow [$dir}]: $file")
            }
          }
          key.reset
        }
      } catch {
        case e: Exception =>
          logger.error(s"Error [$dir]", e)
      } finally {
        watcher.close()
      }
    }

  }

  def start(): FileMonitor = {
    if (!started) synchronized {
      logger.info(s"Starting [$dir]")
      watch(dir)
      thread.start()
      started = true
    }
    this
  }

  def close(): Unit = {
    logger.info(s"Closing [$dir]")
    watcher.close()
    thread.interrupt()
  }

  private def handle(e: FileEvent): Unit =
    try handler(e)
    catch {
      case e: Exception => logger.error("Error", e)
    }

}

object FileMonitor {

  def apply(dir: File, handler: FileEvent => Unit): FileMonitor =
    apply(Paths.get(dir.toURI), handler)

}