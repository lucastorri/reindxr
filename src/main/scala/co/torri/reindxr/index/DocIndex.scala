package co.torri.reindxr.index

import co.torri.reindxr.filemon.DataFile
import java.io.File.{separator => |}
import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions._
import org.apache.lucene.analysis.br.BrazilianAnalyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.Field.Index
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.Field.TermVector
import org.apache.lucene.document.{StringField, Document, Field}
import org.apache.lucene.index.IndexWriterConfig.OpenMode.CREATE_OR_APPEND
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.vectorhighlight.FastVectorHighlighter
import org.apache.lucene.search.vectorhighlight.SimpleFragListBuilder
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.FSDirectory.open
import org.apache.lucene.store.Directory
import org.apache.lucene.util.Version.LUCENE_43
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.metadata.{Metadata => MD}
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.BodyContentHandler
import grizzled.slf4j.Logger
import org.apache.tika.language.LanguageIdentifier
import org.apache.lucene.search.Query
import scala.collection.parallel.ParSeq
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
	val metadata = Map[String, String]()
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

object DocIndex {

  private val preTag = (i: Int) => s"<span class=\"highlight${i}\">"
  private val postTag = (i: Int) => "</span>"
  
  def apply(indexFolder: File, basepath: File) : DocIndex =
    DocIndex(DocIndexConfig(indexFolder, basepath, preTag, postTag, DocFields.identifierField, DocFields.contentField))

}
case class DocIndex(config: DocIndexConfig, searchLimit: Int = 20, highlightLimit: Int = 3, maxNumOfFragment: Int = 1000) {

  import DocFields._
  private val logger = Logger[DocIndex]
  private val docFactory = config.docFactory
  private val snippetHighlighter = config.highlighter(true)
  private val fullDocHighlighter = config.highlighter(false)

  def insert(doc: Doc) : Unit =
    withIndex { index =>
      val newer = index.searchId(doc.id).map(e => doc.newerThan(docFactory(e))).getOrElse(true)
      if (newer) {
        index.delete(doc)
        index.insert(doc)
        logger.info(s"inserting ${doc.id} [${doc.language}]")
      }
    }
    .getOrHandleException { e =>
      logger.error(s"Error when inserting ${doc.id}", e)
    }

  def remove(doc: Doc) : Unit =
    withIndex { index =>
      index.delete(doc)
      logger.info(s"removing ${doc.id}")
    }
    .getOrHandleException { e =>
      logger.error(s"Error when deleting ${doc.id}", e)
    }

    def search(query: String) : Seq[DocMatch] =
      withIndex { index =>
        index.search(query, searchLimit).map{ r =>
          DocMatch(docFactory(r.document))
        }.seq
      }
      .getOrHandleException { e =>
        logger.error(s"Error when searching for ${query}", e)
        Seq()
      }
        
    def snippets(query: String) : Seq[DocMatch] = 
      withIndex { index =>
        index.search(query, searchLimit).map(snippets(_)).seq
      }
      .getOrHandleException { e =>
        logger.error(s"Error when searching for ${query}", e)
        Seq()
      }

    def snippets(id: String, query: String) : DocMatch =
      withIndex { index =>
        index.searchInId(id, query).map(snippets(_)).getOrElse(DocMatch(NullDoc(id)))
      }
      .getOrHandleException { e =>
        logger.error(s"Error when searching for ${query}", e)
        DocMatch(NullDoc(id))
      }
        
    private def snippets(r: SearchResult) = {
      val fq = snippetHighlighter.getFieldQuery(r.q)
      val fragments = snippetHighlighter
        .getBestFragments(fq, r.reader, r.docId, contentField, maxNumOfFragment, highlightLimit)
		  DocMatch(docFactory(r.document), fragments)
    }
        
  def highlight(id: String, query: String) : DocMatch =
    withIndex { index =>
      index.searchInId(id, query).map { r =>
        val fq = fullDocHighlighter.getFieldQuery(r.q)
        val hls = fullDocHighlighter.getBestFragments(fq, r.reader, r.docId, contentField, Int.MaxValue, highlightLimit)
        val s = hls.headOption.getOrElse(docFactory(r.document).contents)
        DocMatch(docFactory(r.document), List(s))
      }.getOrElse(DocMatch(NullDoc(id)))
    }
    .getOrHandleException { e =>
      logger.error(s"Error when highlighting ${id} with query ${query}", e)
      DocMatch(NullDoc(id))
    }

  abstract class IndexRet[R] {
    def getOrHandleException(error: Exception => R) : R
  }

  private def withIndex[R](success: IndexAdapter => R) : IndexRet[R] = try {
    new IndexRet[R] {
      def getOrHandleException(error: Exception => R) : R = success(config.indexAdapter)
    }
  } catch {
    case e: Exception =>
      new IndexRet[R] {
        def getOrHandleException(error: (Exception) => R): R = error(e)
      }
  }

  def close =
    config.close

}

trait IndexAdapter {
  def insert(doc: Doc) : Unit
  def search(query: String, limit: Int) : ParSeq[SearchResult]
  def searchId(id: String) : Option[Document]
  def searchInId(id: String, query: String) : Option[SearchResult]
  def delete(doc: Doc) : Unit
  def close : Unit
}

case class SearchResult(score: Float, q: Query, reader: IndexReader, document: Document, docId: Int)

case class DocIndexConfig(indexpath: File, basepath: File, preTag: Int => String, postTag: Int => String, idField: String, contentField: String) {

  val version = LUCENE_43

  private val idQueryParser =
    queryParser(idField, new KeywordAnalyzer)

  private def fq(field: String, q: String) =
    "%s:(%s)".format(field, q)

  private lazy val writers : ParSeq[IndexWriter] =
    langs.values.toList.map(_.writer).par

  private lazy val analyzers : ParSeq[LangDocIndexConfig] =
    langs.values.toList.par

  val langs = {
    val defaultFactory = LangDocIndexConfig("en", new EnglishAnalyzer(version))
    Map(
      defaultFactory.toPair,
      LangDocIndexConfig("pt", new BrazilianAnalyzer(version)).toPair
    ).withDefaultValue(defaultFactory)
  }

  private def queryParser(fieldName: String, analyzer: Analyzer) = {
    val parser = new QueryParser(version, fieldName, analyzer)
    parser.setDefaultOperator(QueryParser.Operator.AND)
    parser
  }

  def highlighter(snippetsOnly: Boolean) = {
    val fragListBuilder = new SimpleFragListBuilder
    val fragBuilder = TagFragmentBuilder(docFactory, snippetsOnly, preTag, postTag)
    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
  }

  lazy val indexAdapter =
    new DefaultIndexAdapter

  lazy val docFactory =
    Doc.factory(basepath)

  def close =
    analyzers.foreach(_.close)

  class DefaultIndexAdapter extends IndexAdapter {

    private val logger = Logger[DefaultIndexAdapter]

    def insert(doc: Doc) : Unit = {
      val w = langs(doc.language).writer
      w.addDocument(doc.document)
      w.commit
    }

    def search(query: String, limit: Int) : ParSeq[SearchResult] =
      try analyzers.flatMap { a =>
        val q = a.parser.parse(fq(contentField, query))
        a.searcher.search(q, limit).scoreDocs.map {d =>
          SearchResult(d.score, q, a.searcher.getIndexReader, a.searcher.doc(d.doc), d.doc)
        }
      } catch {
        case e: Exception =>
          logger.error("Error when searching", e)
          ParSeq()
      }

    def searchId(id: String) : Option[Document] =
      searchInId(id, id).map(r => r.document)

    def searchInId(id: String, query: String) : Option[SearchResult] = try {
      val q = idQueryParser.parse(fq(idField, s"\"${id}\""))
      analyzers.flatMap { a =>
        a.searcher.search(q, 1).scoreDocs.map { d =>
          val doc = a.searcher.doc(d.doc)
          val q = a.parser.parse(fq(contentField, query))
          SearchResult(d.score, q, a.searcher.getIndexReader, doc, d.doc)
        }
      }.seq.headOption
    } catch {
      case e: Exception =>
        logger.error("Error when id searching", e)
        None
    }

    def delete(doc: Doc) : Unit =
      writers.foreach(_.deleteDocuments(new Term(idField, doc.id)))

    def close =
      langs.values.foreach(_.close)
  }

  object LangDocIndexConfig {
    def apply(lang: String, analyzer: Analyzer) = {
      val dir = new File(indexpath.getAbsolutePath + | + lang)
      dir.mkdirs
      new LangDocIndexConfig(lang, analyzer, open(dir))
    }
  }
  class LangDocIndexConfig(val lang: String, val analyzer: Analyzer, dir: Directory) {

    val writer = {
      val config = new IndexWriterConfig(version, analyzer).setOpenMode(CREATE_OR_APPEND)
      new IndexWriter(dir, config)
    }

    def searcher = new IndexSearcher(DirectoryReader.open(writer, true))

    lazy val parser = queryParser(contentField, analyzer)

    def indexExists =
      DirectoryReader.indexExists(dir)

    def close =
      try { writer.close } catch { case e: Exception => }

    def toPair = (lang, this)
  }
}
