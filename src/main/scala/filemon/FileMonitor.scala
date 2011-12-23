package co.torri.reindxr.filemon

import scala.annotation.tailrec
import java.io._
import Thread.sleep


sealed trait FileEvent {
	def file: File
}
case class FileCreated(file: File) extends FileEvent
case class FileDeleted(file: File) extends FileEvent
case class FileModified(file: File) extends FileEvent

case class FileMonitor(dir: File, callback: FileEvent => Unit) {
	
	private type FileStamp = Map[String, Long]

	private val checkInterval = 10000

	private def findDeletedFiles(original: FileStamp, current: FileStamp) =
		original.keys.filter(k => !current.contains(k)).foreach(k => callback(FileDeleted(new File(k))))
	
	@tailrec
	private def realCheck(files: FileStamp) : Unit = {
        val updatedFiles = 
			dir.
          		listFiles.
            	map { f =>
              		val (filepath, lastModified) = (f.getCanonicalPath, f.lastModified)
              	  	files.get(filepath) match {
                		case Some(oldLastModified) if lastModified == oldLastModified  => ()
						case None => callback(FileCreated(f))
                		case _ => callback(FileModified(f))
              	}
              	(filepath, lastModified)
            }.
            toMap
		
		findDeletedFiles(files, updatedFiles)
				
		sleep(checkInterval)
				
		realCheck(updatedFiles)
	}

	def start =
		realCheck(Map()) // save and get from, maybe, lucene the last known time that the file was modified
}