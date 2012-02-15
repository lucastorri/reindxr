package co.torri.reindxr.index

import java.io.File
import java.io.File.{separator => |}
import java.io.StringReader

import scala.io.Source.fromFile

import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.KeywordAnalyzer
import org.apache.lucene.analysis.SimpleAnalyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.Field.Index
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.Field.TermVector
import org.apache.lucene.index.IndexWriterConfig.OpenMode.CREATE_OR_APPEND
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.FSDirectory.open
import org.apache.lucene.store.Directory
import org.apache.lucene.search.vectorhighlight.FastVectorHighlighter
import org.apache.lucene.search.vectorhighlight.SimpleFragListBuilder
import org.apache.lucene.util.Version.LUCENE_31

import grizzled.slf4j.Logger

case class FileDoc(basepath: String, file: File) {
  lazy val id = Some(file.getAbsolutePath.replace(basepath, "")).map { id =>
    if (id.startsWith(|)) id.replace(|, "") else id
  }.get
  def timestamp = file.lastModified
  def contents = fromFile(file, "UTF-8").mkString
}

object FilesIndex {

  def indexFrom(indexFolder: File) =
    FilesIndex(IndexFactory(open(indexFolder)))

}
case class FilesIndex(factory: IndexFactory) {

  private val logger = Logger[FilesIndex]
    private val searchLimit = 20
  private val highlightLimit = 3
    private val identifierField = "id"
  private val timestampField = "timestamp"
    private val contentField = "contents"
    private val queryParser = factory.newQueryParser(contentField)
  private val idSearchQueryParser = factory.newQueryParser(identifierField, factory.newKeywordAnalyzer)

  private implicit lazy val writer = factory.newWriter

  private def withWriter(exec: IndexWriter => Unit)(implicit writer: IndexWriter) = {
      exec(writer)
    writer.commit
  }

  def insert(file: FileDoc) : Unit = doInsert(file)
    private def doInsert(file: FileDoc) : Unit = try withWriter { writer =>

    if (factory.indexExists && file.timestamp <= timestampFor(file)) {
      logger.info("Already latest version")
      return
    }
        doRemove(file)
       writer.addDocument(file)

    } catch { case e => logger.error("Error when indexing " + file, e) }

  def remove(file: FileDoc) : Unit = doRemove(file)
  private def doRemove(file: FileDoc) = if (factory.indexExists) try withWriter { writer =>

     writer.deleteDocuments(new Term(identifierField, file.id))

  } catch { case e => logger.error("Error when indexing", e) }

  private def withSearcher[T](exec: IndexSearcher => T) = {
      val searcher = factory.newSearcher
      val ret = exec(searcher)
      searcher.close
      ret
  }

  private def timestampFor(f: FileDoc) : Long = withSearcher { searcher =>
    if (!factory.indexExists) {
      logger.info("File not indexed")
      return 0L
    }

    searcher.search(queryParser.parse(identifierField + ":" + f.id), searchLimit)
      .scoreDocs.map(d => searcher.doc(d.doc)).
      headOption.map(_.getFieldable(timestampField).stringValue.toLong).getOrElse(0L)
  }

    def search(query: String): List[(String, List[String])] = try withSearcher { searcher =>

      if (!factory.indexExists) {
          logger.info("Index doesn't exist")
          return List()
      }
    val q = queryParser.parse(contentField + ":" + query)
      val results = searcher.search(q, searchLimit)
    val highlighter = factory.newHighlighter(true)
        val fq = highlighter.getFieldQuery(q)
      val files = results.scoreDocs.sortBy(- _.score).map(_.doc).distinct.par.map{ docId =>
      (
        searcher.doc(docId).get(identifierField),
        highlighter.getBestFragments(fq, searcher.getIndexReader, docId, contentField, 1000, highlightLimit).toList
      )
    }

      files.toList

    } catch { case e => logger.error("Error when searching for " + query, e); List() }

  def highlight(query: String, file: String) : String = try withSearcher { searcher =>

      if (!factory.indexExists) {
          logger.info("Index doesn't exist")
          return ""
      }

    val q = idSearchQueryParser.parse(identifierField + ":" + file)
      val results = searcher.search(q, searchLimit)
    val highlighter = factory.newHighlighter(false)

    if (results.scoreDocs.isEmpty) {
      return ""
    }

    val result =
      if (query.trim.isEmpty) None
      else {
        val fq = highlighter.getFieldQuery(queryParser.parse(contentField + ":" + query))
        results.scoreDocs.headOption.flatMap { result =>
          val hls = highlighter.getBestFragments(fq, searcher.getIndexReader, result.doc, contentField, Int.MaxValue, highlightLimit)
          hls.headOption
        }
      }

    result.orElse(Option(searcher.getIndexReader.document(results.scoreDocs.head.doc).get(contentField))).getOrElse("")

  } catch { case e => logger.error("Error when highlighting " + file + "with query " + query, e); "" }

  private implicit def file2Document(file: FileDoc) : Document = {
    val doc = new Document
    doc.add(new Field(identifierField, file.id, Store.YES, Index.NOT_ANALYZED))
    doc.add(new Field(timestampField, file.timestamp.toString, Store.YES, Index.NOT_ANALYZED))
    doc.add(new Field(contentField, file.contents, Store.YES, Index.ANALYZED, TermVector.WITH_POSITIONS_OFFSETS))
    doc
  }

  def close = {
    writer.close
    factory.close
  }

}

case class IndexFactory(indexPath: Directory) {

  private val version = LUCENE_31
  private val preTag = "<span class=\"highlight"+(_:Int)+"\">";
  private val postTag = "</span>";

  def newAnalyzer =
    new EnglishAnalyzer(version)

  def newKeywordAnalyzer =
    new KeywordAnalyzer

  def config =
    new IndexWriterConfig(version, newAnalyzer).setOpenMode(CREATE_OR_APPEND)

  def newWriter =
    new IndexWriter(indexPath, config)

  def newSearcher =
    new IndexSearcher(indexPath)

  def newQueryParser(fieldName: String, analyzer: Analyzer = newAnalyzer) = {
    val parser = new QueryParser(version, fieldName, analyzer)
    parser.setDefaultOperator(QueryParser.AND_OPERATOR)
    parser
  }

  def newHighlighter(snippetsOnly: Boolean) = {
    val fragListBuilder = new SimpleFragListBuilder
    val fragBuilder = TagFragmentBuilder(snippetsOnly, preTag, (i) => postTag)
    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
  }

  def indexExists =
    IndexReader.indexExists(indexPath)

  def close =
    indexPath.close
}
