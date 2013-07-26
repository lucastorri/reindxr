package co.torri.reindxr.users


import java.nio.file.{Paths, Path}
import java.nio.file.StandardWatchEventKinds._
import co.torri.reindxr.filemon._
import scala.collection.JavaConversions._
import scala.collection.mutable
import co.torri.reindxr.index.{Doc, DocIndex}
import java.io.File
import co.torri.reindxr.filemon.FileModified
import co.torri.reindxr.filemon.FileDeleted
import co.torri.reindxr.filemon.FileCreated


case class UsersMonitor(dataDir: Path, indexDir: Path) {

  private val users = mutable.Map[String, (FileMonitor, DocIndex)]()
  private val watcher = dataDir.getFileSystem.newWatchService()

  //TODO fire events for files that already exist

  dataDir.register(watcher, ENTRY_CREATE, ENTRY_DELETE, OVERFLOW)

  private val thread = new Thread {

    override def run = {
      try {
        while (!Thread.currentThread().isInterrupted) {
          val key = watcher.take
          key.pollEvents.foreach { e =>
            val relativePath = e.context().asInstanceOf[Path]
            val path = key.watchable().asInstanceOf[Path].resolve(relativePath)
            val file = path.toFile
            val username = file.getName
            println("user", e.kind, path.toFile)
            if (file.isDirectory) e.kind match {
              case ENTRY_CREATE =>
                val userIndexDir = new File(indexDir.toFile, username)
                userIndexDir.mkdir()
                val userDataDir = file
                val userIndex = DocIndex(userIndexDir, userDataDir)
                val userMonitor = FileMonitor(file, handler(userIndex, userDataDir))
                users += (username -> (userMonitor.start, userIndex))
              case ENTRY_DELETE =>
                users.remove(username).foreach { case (m, i) =>
                  m.close
                  i.close
                }
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

  private def handler(index: DocIndex, dataDir: File) : FileEvent => Unit = {
    case FileCreated(df) => index.insert(Doc(dataDir, df))
    case FileModified(df) => index.insert(Doc(dataDir, df))
    case FileDeleted(df) => index.remove(Doc(dataDir, df))
  }

  def index(username: String) : Option[DocIndex] =
    users.get(username).map { case (m, i) => i }

  def start() =
    thread.start()

  def close() =
    thread.interrupt() //TODO close each of the users
}
object UsersMonitor {

  def apply(dataDir: File, indexDir: File): UsersMonitor =
    apply(Paths.get(dataDir.toURI), Paths.get(indexDir.toURI))

}