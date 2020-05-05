package co.torri.reindxr.take2

import java.io.InputStream

import cats.effect.IO

trait Document {
  def id: DocumentId

  def content: IO[InputStream]

  def metadata: Map[String, String]
}
