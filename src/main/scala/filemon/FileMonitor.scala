package co.torri.reindxr.filemon

import java.io.File
import java.lang.Thread.sleep

import scala.Array.canBuildFrom
import scala.annotation.tailrec


sealed trait FileEvent {
	def file: File
}
case class FileCreated(file: File) extends FileEvent
case class FileDeleted(file: File) extends FileEvent
case class FileModified(file: File) extends FileEvent

case class FileMonitor(dir: File, callback: FileEvent => Unit) {
	
    private var run = true;
    
	private type FileStamp = Map[String, Long]

	private val checkInterval = 600000

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
				
		if (run) realCheck(updatedFiles)
	}
	
	def stop =
	    run = false

	def start =
		realCheck(Map()) // save and get from, maybe, lucene the last known time that the file was modified
}