package co.torri.reindxr.filemon

import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConversions._
import java.io.File


case class FileMonitor(dir: Path, handler: FileEvent => Unit) extends AutoCloseable {

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
            println("file", e.kind, path.toFile)
            e.kind match {
              case ENTRY_CREATE =>
                watch(path)
                if (isFile) handler(FileCreated(file))
              case ENTRY_MODIFY =>
                if (isFile) handler(FileModified(file))
              case ENTRY_DELETE =>
                if (isFile) handler(FileDeleted(file))
              case OVERFLOW =>
                println(s"overflow on ${path}")
            }
          }
          key.reset
        }
      } catch {
        case e: Exception =>
          e.printStackTrace
      } finally {
        watcher.close()
      }
    }

  }

  def start() = {
    thread.start()
    this
  }

  def close() =
    thread.interrupt()

}
object FileMonitor {

  def apply(dir: File, handler: FileEvent => Unit): FileMonitor =
    apply(Paths.get(dir.toURI), handler)

}