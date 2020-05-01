package co.torri.reindxr.index

import java.io.File
import java.io.File.{separator => |}

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.br.BrazilianAnalyzer
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.index.IndexWriterConfig.OpenMode.CREATE_OR_APPEND
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.vectorhighlight.{FastVectorHighlighter, SimpleFragListBuilder}
import org.apache.lucene.search.{IndexSearcher, Query}
import org.apache.lucene.store.Directory
import org.apache.lucene.store.FSDirectory.open
import org.apache.lucene.util.Version.LUCENE_44

import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq


object DocIndex {

  private val preTag = (i: Int) => s"""<span class="highlight-$i">"""
  private val postTag = (_: Int) => "</span>"

  def apply(indexDir: File, dataDir: File): DocIndex =
    DocIndex(DocIndexConfig(indexDir, dataDir, preTag, postTag, DocFields.identifierField, DocFields.contentField))

}

case class DocIndex(config: DocIndexConfig, searchLimit: Int = 20, highlightLimit: Int = 3, maxNumOfFragment: Int = 1000) extends LazyLogging {

  import DocFields._

  private val docFactory = config.docFactory
  private val snippetHighlighter = config.highlighter(true)
  private val fullDocHighlighter = config.highlighter(false)

  def insert(doc: Doc): Unit =
    withIndex { index =>
      val newer = index.searchId(doc.id).forall(e => doc.newerThan(docFactory(e)))
      if (newer) {
        index.delete(doc)
        index.insert(doc)
        logger.debug(s"inserting ${doc.id} [${doc.language}]")
      }
    }
      .getOrHandleException { e =>
        logger.error(s"Error when inserting ${doc.id}", e)
      }

  def remove(doc: Doc): Unit =
    withIndex { index =>
      index.delete(doc)
      logger.debug(s"removing ${doc.id}")
    }
      .getOrHandleException { e =>
        logger.error(s"Error when deleting ${doc.id}", e)
      }

  def search(query: String): Seq[DocMatch] =
    withIndex { index =>
      index.search(query, searchLimit).map { r =>
        DocMatch(docFactory(r.document))
      }.seq.toSeq
    }
      .getOrHandleException { e =>
        logger.error(s"Error when searching for $query", e)
        Seq.empty
      }

  def snippets(query: String): Seq[DocMatch] =
    withIndex { index =>
      index.search(query, searchLimit).map(snippets).seq.toSeq
    }
      .getOrHandleException { e =>
        logger.error(s"Error when searching for $query", e)
        Seq.empty
      }

  def snippets(id: String, query: String): DocMatch =
    withIndex { index =>
      index.searchInId(id, query).map(snippets).getOrElse(DocMatch(NullDoc(id)))
    }
      .getOrHandleException { e =>
        logger.error(s"Error when searching for $query", e)
        DocMatch(NullDoc(id))
      }

  private def snippets(r: SearchResult): DocMatch = {
    val fq = snippetHighlighter.getFieldQuery(r.q)
    val fragments = snippetHighlighter
      .getBestFragments(fq, r.reader, r.docId, contentField, maxNumOfFragment, highlightLimit)
    DocMatch(docFactory(r.document), fragments)
  }

  def highlight(id: String, query: String): DocMatch =
    withIndex { index =>
      index.searchInId(id, query).map { r =>
        val fq = fullDocHighlighter.getFieldQuery(r.q)
        val hls = fullDocHighlighter.getBestFragments(fq, r.reader, r.docId, contentField, Int.MaxValue, highlightLimit)
        val s = hls.headOption.getOrElse(docFactory(r.document).contents)
        DocMatch(docFactory(r.document), List(s))
      }.getOrElse(DocMatch(NullDoc(id)))
    }
      .getOrHandleException { e =>
        logger.error(s"Error when highlighting $id with query $query", e)
        DocMatch(NullDoc(id))
      }

  abstract class IndexRet[R] {
    def getOrHandleException(error: Exception => R): R
  }

  private def withIndex[R](success: IndexAdapter => R): IndexRet[R] = try {
    (_: Exception => R) => success(config.indexAdapter)
  } catch {
    case e: Exception =>
      (error: Exception => R) => error(e)
  }

  def close(): Unit =
    config.close()

}

trait IndexAdapter {
  def insert(doc: Doc): Unit

  def search(query: String, limit: Int): ParSeq[SearchResult]

  def searchId(id: String): Option[Document]

  def searchInId(id: String, query: String): Option[SearchResult]

  def delete(doc: Doc): Unit

  def close(): Unit
}

case class SearchResult(score: Float, q: Query, reader: IndexReader, document: Document, docId: Int)

case class DocIndexConfig(indexDir: File, dataDir: File, preTag: Int => String, postTag: Int => String, idField: String, contentField: String) extends LazyLogging {

  val version = LUCENE_44

  private val idQueryParser =
    queryParser(idField, new KeywordAnalyzer)

  private def fq(field: String, q: String): String =
    "%s:(%s)".format(field, q)

  private lazy val writers: ParSeq[IndexWriter] =
    languages.values.toList.map(_.writer).par

  private lazy val analyzers: ParSeq[LanguageConfig] =
    languages.values.toList.par

  val languages: Map[String, LanguageConfig] = {
    val default = LanguageConfig("en", new EnglishAnalyzer(version))
    Map(
      default.toPair,
      LanguageConfig("pt", new BrazilianAnalyzer(version)).toPair
    ).withDefaultValue(default)
  }

  private def queryParser(fieldName: String, analyzer: Analyzer) = {
    val parser = new QueryParser(version, fieldName, analyzer)
    parser.setDefaultOperator(QueryParser.Operator.AND)
    parser
  }

  def highlighter(snippetsOnly: Boolean): FastVectorHighlighter = {
    val fragListBuilder = new SimpleFragListBuilder
    val fragBuilder = TagFragmentBuilder(docFactory, snippetsOnly, preTag, postTag)
    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
  }

  lazy val indexAdapter: DefaultIndexAdapter =
    new DefaultIndexAdapter

  lazy val docFactory: Docs =
    Doc.docs(dataDir)

  def close(): Unit =
    analyzers.foreach(_.close())

  class DefaultIndexAdapter extends IndexAdapter with LazyLogging {

    def insert(doc: Doc): Unit = {
      val w = languages(doc.language).writer
      w.addDocument(doc.document)
      w.commit()
    }

    def search(query: String, limit: Int): ParSeq[SearchResult] =
      try analyzers.flatMap { a =>
        val q = a.parser.parse(fq(contentField, query))
        a.searcher.search(q, limit).scoreDocs.map { d =>
          SearchResult(d.score, q, a.searcher.getIndexReader, a.searcher.doc(d.doc), d.doc)
        }
      } catch {
        case e: Exception =>
          logger.error("Error when searching", e)
          ParSeq()
      }

    def searchId(id: String): Option[Document] =
      searchInId(id, id).map(r => r.document)

    def searchInId(id: String, query: String): Option[SearchResult] = try {
      val q = idQueryParser.parse(fq(idField, s""" "$id" """.trim))
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

    def delete(doc: Doc): Unit =
      writers.foreach(_.deleteDocuments(new Term(idField, doc.id)))

    def close(): Unit =
      languages.values.foreach(_.close())
  }

  object LanguageConfig {
    def apply(lang: String, analyzer: Analyzer): LanguageConfig = {
      val dir = new File(indexDir.getAbsolutePath + | + lang)
      dir.mkdirs
      new LanguageConfig(lang, analyzer, open(dir))
    }
  }

  class LanguageConfig(val lang: String, val analyzer: Analyzer, dir: Directory) {

    val writer: IndexWriter = {
      val config = new IndexWriterConfig(version, analyzer).setOpenMode(CREATE_OR_APPEND)
      new IndexWriter(dir, config)
    }

    def searcher: IndexSearcher = new IndexSearcher(DirectoryReader.open(writer, true))

    lazy val parser: QueryParser = queryParser(contentField, analyzer)

    def indexExists: Boolean =
      DirectoryReader.indexExists(dir)

    def close(): Unit =
      try {
        writer.close()
      } catch {
        case e: Exception =>
          logger.error("Failed to close language config", e)
      }

    def toPair: (String, LanguageConfig) = (lang, this)
  }

}

trait DocIndexes {
  def index(username: String): Option[DocIndex]
}