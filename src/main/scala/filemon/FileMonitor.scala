package co.torri.reindxr

import java.io.File.{separator => |}
import java.io.File
import java.lang.Thread.sleep

import scala.Array.canBuildFrom
import scala.annotation.tailrec


package object filemon {

	type Handler = FileEvent => Unit
  
	val metadataExtension = ".metadata"
	implicit def dataFile2creator(f: DataFile) = new FileEventDispatcher(f)  
	implicit def file2creator(f: File) = dataFile2creator(DataFile(f))
	implicit def string2creator(s: String) = file2creator(new File(s))
	
	sealed trait FileEvent
	case class FileCreated(file: DataFile) extends FileEvent
	case class FileDeleted(file: DataFile) extends FileEvent
	case class FileModified(file: DataFile) extends FileEvent
	
	object DataFile {
		def apply(filepath: String) : DataFile = apply(new File(filepath))
	}
	case class DataFile(f: File) {
		private def isMetadata = f.getName.endsWith(metadataExtension)
		def isValid = file.exists
		
		val (file, metadata) =
			if (isMetadata) {
				(new File(f.getAbsolutePath.replaceFirst(metadataExtension + "$", "")), f)
			} else {
				(f, new File(f.getAbsolutePath + | + metadataExtension))
			}
	}
	
	case class FileEventDispatcher(f: DataFile) {
		def created(h: Handler) = event(h, FileCreated.apply _)
		def modified(h: Handler) = event(h, FileModified.apply _)
		def deleted(h: Handler) = event(h, FileDeleted.apply _)
		private def event(h: Handler, e : (DataFile) => FileEvent) = if (f.isValid) h(e(f))
	}
	
	case class FileMonitor(dir: File, eventHandler: Handler) {
	    private var run = true;
	    
		private type FileStamp = Map[String, Long]
	
		private val checkInterval = 10000
	
		private def findDeletedFiles(original: FileStamp, current: FileStamp) =
			original.keys.filter(k => !current.contains(k)).foreach(k => k.deleted(eventHandler))
		
		@tailrec
		private def realCheck(files: FileStamp) : Unit = {
	        val updatedFiles = 
				dir.
	                listFiles.
	            	map { f =>
	                    val (filepath, lastModified) = (f.getCanonicalPath, f.lastModified)
	                    files.get(filepath) match {
	                		case Some(oldLastModified) if lastModified == oldLastModified  => ()
							case None => f.created(eventHandler)
	                		case _ => f.modified(eventHandler)
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

}