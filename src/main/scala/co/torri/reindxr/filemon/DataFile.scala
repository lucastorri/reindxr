package co.torri.reindxr.filemon

import java.io.File

case class DataFile private(file: File, metadata: File) {

  def hasMetadata: Boolean = metadata.exists

}

object DataFile {
  val metadataExtension: String = ".metadata"
  val extensionRegex: String = metadataExtension + "$"

  def apply(path: String): DataFile =
    apply(new File(path))

  def apply(f: File): DataFile = {
    val isMetadata = f.getName.endsWith(metadataExtension)

    val (file, metadata) =
      if (isMetadata) (new File(f.getAbsolutePath.replaceFirst(extensionRegex, "")), f)
      else (f, new File(f.getAbsolutePath + metadataExtension))

    DataFile(file, metadata)
  }
}
