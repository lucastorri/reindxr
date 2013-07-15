package co.torri.reindxr.filemon

import java.io.File.{separator => |}
import java.io.File

sealed trait FileEvent
case class FileCreated(file: DataFile) extends FileEvent
case class FileDeleted(file: DataFile) extends FileEvent
case class FileModified(file: DataFile) extends FileEvent

object DataFile {
  def apply(filepath: String) : DataFile = apply(new File(filepath))
}
case class DataFile(f: File) {
  val metadataExtension = ".metadata"

  private def isMetadata = f.getName.endsWith(metadataExtension)
  def isValid = file.exists

  val (file, metadata) =
    if (isMetadata) {
      (new File(f.getAbsolutePath.replaceFirst(metadataExtension + "$", "")), f)
    } else {
      (f, new File(f.getAbsolutePath + | + metadataExtension))
    }
}