package co.torri.reindxr.filemon

import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConversions._
import java.io.File
import grizzled.slf4j.Logging


case class FileMonitor(dir: Path, handler: FileEvent => Unit) extends AutoCloseable with Logging {

  private val watcher = dir.getFileSystem.newWatchService()

  //TODO fire events for files that already exist

  private def watch(path: Path) = {
    def w(path: Path) = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW)
    if (path.toFile.isDirectory) {
      w(path)
      Files.walkFileTree(path, new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attributes: BasicFileAttributes) = {
          w(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
  }

  watch(dir)

  private val thread = new Thread {

    override def run = {
      try {
        while (!Thread.currentThread().isInterrupted) {
          val key = watcher.take
          key.pollEvents.foreach { e =>
            val relativePath = e.context().asInstanceOf[Path]
            val path = key.watchable().asInstanceOf[Path].resolve(relativePath)
            val file = DataFile(path.toFile)
            val isFile = path.toFile.isFile
            e.kind match {
              case ENTRY_CREATE =>
                watch(path)
                if (isFile) {
                  logger.debug(s"File created on FileMonitor[${dir}]: ${file}")
                  handler(FileCreated(file))
                }
              case ENTRY_MODIFY =>
                if (isFile) {
                  logger.debug(s"File modified on FileMonitor[${dir}]: ${file}")
                  handler(FileModified(file))
                }
              case ENTRY_DELETE =>
                if (isFile) {
                  logger.debug(s"File deleted on FileMonitor[${dir}]: ${file}")
                  handler(FileDeleted(file))
                }
              case OVERFLOW =>
                println(s"Overflow on FileMonitor[${dir}}]: ${file}")
            }
          }
          key.reset
        }
      } catch {
        case e: Exception =>
          logger.error(s"Error on FileMonitor[${dir}]", e)
      } finally {
        watcher.close()
      }
    }

  }

  def start() = {
    logger.info(s"Starting FileMonitor[${dir}]")
    thread.start()
    this
  }

  def close() = {
    logger.info(s"Closing FileMonitor[${dir}]")
    watcher.close()
    thread.interrupt()
  }

}
object FileMonitor {

  def apply(dir: File, handler: FileEvent => Unit): FileMonitor =
    apply(Paths.get(dir.toURI), handler)

}