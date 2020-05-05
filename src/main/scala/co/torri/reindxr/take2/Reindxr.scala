package co.torri.reindxr.take2

import cats.effect.IO

trait Reindxr {
  def add(document: Document): IO[Unit]

  def search(query: String): IO[Seq[DocumentMatch]]

  def snippets(query: String): IO[Seq[DocumentMatch]]

  def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]]

  def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]]

  def remove(documentId: DocumentId): IO[Unit]

  def update(updatedDocument: Document): IO[Unit]

  def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit]
}
