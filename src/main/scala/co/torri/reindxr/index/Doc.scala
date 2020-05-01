package co.torri.reindxr.index

import java.io.File.{separator => |}
import java.io.{File, FileInputStream, StringReader}
import java.util.Properties

import co.torri.reindxr.filemon.DataFile
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{Document, StringField, TextField}
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.language.LanguageIdentifier
import org.apache.tika.metadata.{Metadata => MD}
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.BodyContentHandler

import scala.io.Source
import scala.jdk.CollectionConverters._


trait DocFields {
  val identifierField = "id"
  val timestampField = "timestamp"
  val metadataTimestampField = "metadataTimestamp"
  val contentField = "contents"
  val languageField = "lang"

  val fields: Set[String] = Set(identifierField, timestampField, metadataTimestampField, contentField, languageField)
}

object DocFields extends DocFields

trait Doc {
  def id: String

  def timestamp: Long

  def metadataTimestamp: Long

  def contents: String

  def language: String

  def data: DataFile

  def document: Document

  def metadata: Map[String, String]

  def newerThan(d: Doc): Boolean = timestamp > d.timestamp || metadataTimestamp > d.metadataTimestamp
}

object Doc {
  def apply(base: File, f: DataFile) = FileDoc(base.getCanonicalPath, f)

  def apply(base: File, d: Document) = DocumentDoc(base.getCanonicalPath, d)

  def docs(base: File): Docs = new Docs {
    def apply(f: File): Doc = apply(DataFile(f))

    def apply(f: DataFile): FileDoc = Doc(base, f)

    override def apply(d: Document): DocumentDoc = Doc(base, d)
  }
}

sealed trait Docs {
  def apply(f: File): Doc

  def apply(d: Document): Doc
}

case class FileDoc(base: String, data: DataFile) extends Doc with DocConverter with DocReader with MetadataReader {
  override lazy val id: String = {
    val id = data.file.getAbsolutePath.replace(base, "")
    if (id.startsWith(|)) id.replace(|, "") else id
  }

  override def timestamp: Long = data.file.lastModified

  override def metadataTimestamp: Long = if (data.hasMetadata) data.metadata.lastModified else 0L

  override lazy val language: String = new LanguageIdentifier(contents).getLanguage
}

case class DocumentDoc(base: String, document: Document) extends Doc with DocReader with MetadataExtractor {
  override lazy val id: String = document.getField(identifierField).stringValue
  override lazy val timestamp: Long = document.getField(timestampField).stringValue.toLong
  override lazy val metadataTimestamp: Long = document.getField(metadataTimestampField).stringValue.toLong

  override def data: DataFile = DataFile(new File(base + | + id))

  override def language: String = document.getField(languageField).stringValue
}

case class NullDoc(id: String) extends Doc {
  override val timestamp = 0L
  override val metadataTimestamp = 0L
  override val contents = ""
  override val language = ""
  override val data: DataFile = null
  override val document: Document = null
  override val metadata: Map[String, String] = Map[String, String]()
}

trait MetadataReader {
  self: Doc =>

  def metadata: Map[String, String] =
    if (self.data.metadata.exists) {
      val p = new Properties
      p.load(new StringReader(Source.fromFile(self.data.metadata, "utf-8").mkString))
      p.asScala.map { case (k, v) =>
        (k.toString, v.toString)
      }
        .toMap
    } else {
      Map()
    }

}

trait MetadataExtractor extends DocFields {
  self: Doc =>

  def metadata: Map[String, String] =
    document.getFields
      .asScala
      .filterNot(f => fields.contains(f.name))
      .map(f => (f.name, f.stringValue))
      .toMap

}

trait DocConverter extends DocFields {
  self: Doc =>

  def document: Document = {
    val d = new Document
    metadata.foreach { case (field, value) =>
      d.add(new TextField(field, value, Store.YES))
    }
    d.add(new StringField(identifierField, self.id, Store.YES))
    d.add(new StringField(timestampField, self.timestamp.toString, Store.YES))
    d.add(new StringField(metadataTimestampField, self.metadataTimestamp.toString, Store.YES))
    d.add(new StringField(languageField, self.language, Store.YES))
    d.add(new TextWithOffsetsField(contentField, self.contents))
    d
  }

}

trait DocReader {
  self: Doc =>

  private val parser = new AutoDetectParser(new DefaultDetector)

  private def read(f: File): String = {
    val c = new BodyContentHandler(Int.MaxValue)
    val m = new MD
    parser.parse(new FileInputStream(f), c, m)
    c.toString
  }

  override def contents: String =
    read(data.file)
}


case class DocMatch(doc: Doc, matches: Seq[String] = Seq())