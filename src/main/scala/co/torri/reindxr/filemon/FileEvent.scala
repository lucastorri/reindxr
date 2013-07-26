package co.torri.reindxr.filemon

import java.io.File


sealed trait FileEvent
case class FileCreated(file: DataFile) extends FileEvent
case class FileDeleted(file: DataFile) extends FileEvent
case class FileModified(file: DataFile) extends FileEvent