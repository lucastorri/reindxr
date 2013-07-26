package co.torri.reindxr.filemon


sealed trait FileEvent {
  def file: DataFile
}
case class FileCreated(file: DataFile) extends FileEvent
case class FileDeleted(file: DataFile) extends FileEvent
case class FileModified(file: DataFile) extends FileEvent