package co.torri.reindxr.take2

import cats.effect.IO

trait DocumentStore {
  def add(document: Document): IO[Unit]

  def get(documentId: DocumentId): IO[Document]

  def remove(documentId: DocumentId): IO[Unit]

  def update(updatedDocument: Document): IO[Unit]

  def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit]
}
