package co.torri.reindxr.take2

import cats.effect.IO

class ExternalIndexReindxr(store: DocumentStore, index: DocumentIndex) extends Reindxr {

  override def add(document: Document): IO[Unit] =
    for {
      _ <- store.add(document)
      _ <- index.add(document)
    } yield ()

  override def remove(documentId: DocumentId): IO[Unit] =
    for {
      _ <- store.remove(documentId)
      _ <- index.remove(documentId)
    } yield ()

  override def update(updatedDocument: Document): IO[Unit] =
    for {
      _ <- store.update(updatedDocument)
      _ <- index.update(updatedDocument)
    } yield ()

  override def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit] =
    for {
      _ <- store.updateMetadata(documentId, newMetadata)
      _ <- index.updateMetadata(documentId, newMetadata)
    } yield ()

  override def search(query: String): IO[Seq[DocumentMatch]] =
    index.search(query)

  override def snippets(query: String): IO[Seq[DocumentMatch]] =
    index.snippets(query)

  override def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    index.snippets(documentId, query)

  override def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    index.highlight(documentId, query)
}
