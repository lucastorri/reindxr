package co.torri.reindxr.take2

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.Properties

import cats.effect.{IO, Resource}

import scala.jdk.CollectionConverters._


class FilesystemDocumentStore(directory: Path) extends DocumentStore {

  require(Files.isDirectory(directory), s"$directory is not a directory")

  override def get(documentId: DocumentId): IO[Document] =
    for {
      metadata <- metadataFile(documentId).in(propertiesToMetadata)
    } yield new FileDocument(documentId, metadata)

  private def metadataFile(documentId: DocumentId): InternalFile =
    new InternalFile(documentId, ".metadata")

  private def propertiesToMetadata(in: InputStream): Map[String, String] = {
    val properties = new Properties()
    properties.load(in)
    properties.asScala.toMap
  }

  override def remove(documentId: DocumentId): IO[Unit] =
    for {
      _ <- contentFile(documentId).delete()
      _ <- metadataFile(documentId).delete()
    } yield ()

  override def update(updatedDocument: Document): IO[Unit] =
    add(updatedDocument)

  override def add(document: Document): IO[Unit] =
    for {
      contentIn <- document.content
      _ <- contentFile(document.id).out(contentIn.transferTo)
      _ <- updateMetadata(document.id, document.metadata)
    } yield ()

  override def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit] =
    metadataFile(documentId).out(metadataToProperties(newMetadata).store(_, documentId.value))

  private def metadataToProperties(metadata: Map[String, String]): Properties = {
    val properties = new Properties()
    properties.putAll(metadata.asJava)
    properties
  }

  private def contentFile(documentId: DocumentId): InternalFile =
    new InternalFile(documentId, "")

  private class InternalFile(documentId: DocumentId, extension: String) {
    private val path = directory.resolve(s"${idHash(documentId)}$extension")

    def in[A](f: InputStream => A): IO[A] =
      Resource
        .fromAutoCloseable(IO(in))
        .use(in => IO(f(in)))

    def in[A]: InputStream = Files.newInputStream(path)

    def out[A](f: OutputStream => A): IO[A] =
      Resource
        .fromAutoCloseable(IO(Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)))
        .use(out => IO(f(out)))

    def delete(): IO[Unit] = IO(Files.delete(path))

    //TODO
    private def idHash(documentId: DocumentId): String =
      documentId.value.hashCode.toString
  }

  private class FileDocument(documentId: DocumentId, override val metadata: Map[String, String]) extends Document {
    override def id: DocumentId = documentId

    override def content: IO[InputStream] =
      IO(contentFile(documentId).in)
  }

}
