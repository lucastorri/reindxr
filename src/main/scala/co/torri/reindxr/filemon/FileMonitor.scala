package co.torri.reindxr.filemon

import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes
import scala.collection.JavaConversions._
import java.io.File


case class FileMonitor(dir: Path, handler: FileEvent => Unit) extends AutoCloseable {

  private val watcher = dir.getFileSystem.newWatchService()

  private def watch(path: Path) = {
    def w(path: Path) = dir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY, OVERFLOW)

    w(path)
    if (path.toFile.isDirectory) Files.walkFileTree(dir, new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        w(dir)
        FileVisitResult.CONTINUE
      }
    })
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
            e.kind match {
              case ENTRY_CREATE =>
                watch(path)
                handler(FileCreated(file))
              case ENTRY_MODIFY =>
                handler(FileModified(file))
              case ENTRY_DELETE =>
                handler(FileDeleted(file))
              case OVERFLOW =>
                println(s"overflow on ${path}")
            }
            println(path.toFile)
            println(e.kind)
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

  def start() =
    thread.start()

  def close() =
    thread.interrupt()

}
object FileMonitor {

  def apply(dir: File, handler: FileEvent => Unit): FileMonitor =
    apply(Paths.get(dir.toURI), handler)

}