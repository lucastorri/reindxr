package co.torri.reindxr.filemon


case class FileEventDispatcher(f: DataFile) {
  def created(h: FileEvent => Unit) = event(h, FileCreated.apply _)
  def modified(h: FileEvent => Unit) = event(h, FileModified.apply _)
  def deleted(h: FileEvent => Unit) = event(h, FileDeleted.apply _)
  private def event(h: FileEvent => Unit, e : (DataFile) => FileEvent) = if (f.isValid) h(e(f))
}
