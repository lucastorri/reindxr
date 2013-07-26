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
import grizzled.slf4j.Logging


case class UsersMonitor(dataDir: Path, indexDir: Path) extends Logging {

  private type User = (FileMonitor, DocIndex)
  private val users = mutable.Map[String, User]()
  private val watcher = dataDir.getFileSystem.newWatchService()

  dataDir.toFile.listFiles.foreach(addUser)
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
            e.kind match {
              case ENTRY_CREATE => addUser(file)
              case ENTRY_DELETE => removeUser(file)
              case OVERFLOW => logger.error(s"Overflow on ${path}")
            }
          }
          key.reset
        }
      } catch {
        case e: Exception =>
          logger.error("Error", e)
      } finally {
        watcher.close()
      }
    }

  }

  private def addUser(userDataDir: File) = if (userDataDir.isDirectory) {
    val username = userDataDir.getName
    val userIndexDir = new File(indexDir.toFile, username)
    userIndexDir.mkdir()
    val userIndex = DocIndex(userIndexDir, userDataDir)
    val userMonitor = FileMonitor(userDataDir, handler(userIndex, userDataDir))
    users += (username ->(userMonitor.start, userIndex))
    logger.info(s"User added: ${username}")
  }

  private def removeUser(userDataDir: File) = if (userDataDir.isDirectory) {
    val username = userDataDir.getName
    close(users.remove(username))
    logger.info(s"User removed: ${username}")
  }

  def index(username: String) : Option[DocIndex] =
    users.get(username).map { case (m, i) => i }

  def start() = {
    logger.info("Starting")
    thread.start()
  }

  def close() = {
    logger.info("Stopping")
    watcher.close()
    thread.interrupt()
    close(users.values)
  }

  private def close(users: Iterable[User]) =
    users.foreach { case (m, i) =>
      m.close
      i.close
    }

  private def handler(index: DocIndex, dataDir: File) : FileEvent => Unit = {
    case FileCreated(df) => index.insert(Doc(dataDir, df))
    case FileModified(df) => index.insert(Doc(dataDir, df))
    case FileDeleted(df) => index.remove(Doc(dataDir, df))
  }
}
object UsersMonitor {

  def apply(dataDir: File, indexDir: File): UsersMonitor =
    apply(Paths.get(dataDir.toURI), Paths.get(indexDir.toURI))

}