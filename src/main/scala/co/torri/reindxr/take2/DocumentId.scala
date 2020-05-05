package co.torri.reindxr.take2

sealed abstract case class DocumentId(value: String)

object DocumentId {
  def apply(id: String): DocumentId = new DocumentId(id) {}
}