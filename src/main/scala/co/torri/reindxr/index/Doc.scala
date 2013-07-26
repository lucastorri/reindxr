package co.torri.reindxr.index

import co.torri.reindxr.filemon.DataFile
import java.io.File.{separator => |}
import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions._
import org.apache.lucene.document.Field.Index
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.Field.TermVector
import org.apache.lucene.document.{StringField, Document, Field}
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.metadata.{Metadata => MD}
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.BodyContentHandler
import org.apache.tika.language.LanguageIdentifier
import java.util.Properties
import java.io.StringReader
import scala.io.Source


trait DocFields {
  val identifierField = "id"
  val timestampField = "timestamp"
  val metadataTimestampField = "metadataTimestamp"
  val contentField = "contents"
  val languageField = "lang"

  val fields = Set(identifierField, timestampField, metadataTimestampField, contentField, languageField)
}
object DocFields extends DocFields

trait Doc {
  def id : String
  def timestamp : Long
  def metadataTimestamp : Long
  def contents : String
  def language : String

  def data : DataFile
  def document : Document
  def metadata : Map[String, String]

  def newerThan(d: Doc) = timestamp > d.timestamp || metadataTimestamp > d.metadataTimestamp
}
object Doc {
  private implicit def file2string(f: File) = f.getCanonicalPath
  def apply(basepath: File, f: DataFile) = FileDoc(basepath, f)
  def apply(basepath: File, d: Document) = DocumentDoc(basepath, d)
  def factory(basepath: File) : DocFactory = new DocFactory {
    def apply(f: File) = apply(DataFile(f))
    def apply(f: DataFile) = Doc(basepath, f)
    def apply(d: Document) = Doc(basepath, d)
  }
}

trait DocFactory {
  def apply(f: File) : Doc
  def apply(d: Document) : Doc
}

case class FileDoc(basepath: String, data: DataFile) extends Doc with DocConverter with DocReader with MetadataReader {
  lazy val id = {
    val id = data.file.getAbsolutePath.replace(basepath, "")
    if (id.startsWith(|)) id.replace(|, "") else id
  }
  def timestamp = data.file.lastModified
  def metadataTimestamp = if (data.metadata.exists) data.metadata.lastModified else 0L
  lazy val language = new LanguageIdentifier(contents).getLanguage
}

case class DocumentDoc(basepath: String, document: Document) extends Doc with DocReader with MetadataExtractor {
  lazy val id = document.getField(identifierField).stringValue
  lazy val timestamp = document.getField(timestampField).stringValue.toLong
  lazy val metadataTimestamp = document.getField(metadataTimestampField).stringValue.toLong
  def data = DataFile(new File(basepath + | + id))
  def language = document.getField(languageField).stringValue
}

case class NullDoc(id: String) extends Doc {
  val timestamp = 0L
  val metadataTimestamp = 0L
  val contents = ""
  val language = ""
  val data = null
  val document = null
  val metadata = Map()
}

trait MetadataReader { self : Doc =>

  def metadata : Map[String, String] =
    if (self.data.metadata.exists) {
      val p = new Properties
      p.load(new StringReader(Source.fromFile(self.data.metadata, "utf-8").mkString))
      p.map { case (k,v) =>
        (k.toString, v.toString)
      }
      .toMap
    } else {
      Map()
    }

}

trait MetadataExtractor extends DocFields { self: Doc =>

  def metadata : Map[String, String] =
    document.getFields
      .filterNot(f => fields.contains(f.name))
      .map(f => (f.name, f.stringValue))
      .toMap

}

trait DocConverter extends DocFields { self : Doc =>

  def document = {
    val d = new Document
    metadata.foreach { case (field, value) =>
      d.add(new Field(field, value, Store.YES, Index.ANALYZED))
    }
    d.add(new StringField(identifierField, self.id, Store.YES))
    d.add(new StringField(timestampField, self.timestamp.toString, Store.YES))
    d.add(new StringField(metadataTimestampField, self.metadataTimestamp.toString, Store.YES))
    d.add(new StringField(languageField, self.language, Store.YES))
    d.add(new Field(contentField, self.contents, Store.NO, Index.ANALYZED, TermVector.WITH_POSITIONS_OFFSETS))
    d
  }

}

trait DocReader { self : Doc =>
  private val parser = new AutoDetectParser(new DefaultDetector)

  private def read(f: File) : String = {
    val c = new BodyContentHandler(Int.MaxValue)
    val m = new MD
    parser.parse(new FileInputStream(f), c, m)
    c.toString
  }

  override def contents =
    read(data.file)
}


case class DocMatch(doc: Doc, matches: Seq[String] = List())