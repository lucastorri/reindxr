package co.torri.reindxr.filemon

import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConversions._
import java.io.File
import com.typesafe.scalalogging.slf4j.Logging
import java.nio.file.FileVisitResult.CONTINUE


case class FileMonitor(dir: Path, handler: FileEvent => Unit) extends AutoCloseable with Logging {

  private var started = false
  private val watcher = dir.getFileSystem.newWatchService()

  private def watch(path: Path) = {
    def w(path: Path) = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW)
    if (path.toFile.isDirectory) {
      w(path)
      Files.walkFileTree(path, new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attributes: BasicFileAttributes) = {
          w(dir)
          CONTINUE
        }
        override def visitFile(file: Path, attributes: BasicFileAttributes) = {
          logger.debug(s"File exists [${dir}]: ${file}")
          handle(FileCreated(DataFile(file.toFile)))
          CONTINUE
        }
      })
    }
  }

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
                  logger.debug(s"File created [${dir}]: ${file}")
                  handle(FileCreated(file))
                }
              case ENTRY_MODIFY =>
                if (isFile) {
                  logger.debug(s"File modified [${dir}]: ${file}")
                  handle(FileModified(file))
                }
              case ENTRY_DELETE =>
                if (isFile) {
                  logger.debug(s"File deleted [${dir}]: ${file}")
                  handle(FileDeleted(file))
                }
              case OVERFLOW =>
                logger.error(s"Overflow [${dir}}]: ${file}")
            }
          }
          key.reset
        }
      } catch {
        case e: Exception =>
          logger.error(s"Error [${dir}]", e)
      } finally {
        watcher.close()
      }
    }

  }

  def start() = {
    if (!started) synchronized {
      logger.info(s"Starting [${dir}]")
      watch(dir)
      thread.start()
      started = true
    }
    this
  }

  def close() = {
    logger.info(s"Closing [${dir}]")
    watcher.close()
    thread.interrupt()
  }

  private def handle(e: FileEvent) =
    try handler(e)
    catch {
      case e: Exception => logger.error("Error", e)
    }

}
object FileMonitor {

  def apply(dir: File, handler: FileEvent => Unit): FileMonitor =
    apply(dir.toPath, handler)

}