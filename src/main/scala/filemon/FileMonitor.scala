package co.torri.reindxr.filemon

import java.io.File
import annotation.tailrec
import java.lang.Thread._
import scala.Some

case class FileMonitor(dir: File, eventHandler: FileEvent => Unit) {
  private var run = true;

  private type FileStamp = Map[String, Long]

  private val checkInterval = 10000

  private def findDeletedFiles(original: FileStamp, current: FileStamp) =
    original.keys.filter(k => !current.contains(k)).foreach(k => FileEventDispatcher(DataFile(k)).deleted(eventHandler))

  @tailrec
  private def realCheck(files: FileStamp) : Unit = {
    val updatedFiles =
      dir.
        listFiles.
        map { f =>
        val (filepath, lastModified) = (f.getCanonicalPath, f.lastModified)
        files.get(filepath) match {
          case Some(oldLastModified) if lastModified == oldLastModified  => ()
          case None => FileEventDispatcher(DataFile(f)).created(eventHandler)
          case _ => FileEventDispatcher(DataFile(f)).modified(eventHandler)
        }
        (filepath, lastModified)
      }.
        toMap

    findDeletedFiles(files, updatedFiles)

    sleep(checkInterval)

    if (run) realCheck(updatedFiles)
  }

  def stop =
    run = false

  def start =
    realCheck(Map())
}
