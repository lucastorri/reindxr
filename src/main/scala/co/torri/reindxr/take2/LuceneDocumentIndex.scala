package co.torri.reindxr.take2

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.implicits._
import co.torri.reindxr.index.{SearchResult, TextWithOffsetsField}
import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.br.BrazilianAnalyzer
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{StringField, Document => LuceneDocument}
import org.apache.lucene.index.IndexWriterConfig.OpenMode.CREATE_OR_APPEND
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.flexible.standard.QueryParserUtil
import org.apache.lucene.search.vectorhighlight.{FastVectorHighlighter, SimpleFragListBuilder}
import org.apache.lucene.search.{BooleanClause, BooleanQuery, IndexSearcher, Query}
import org.apache.lucene.store.{Directory, FSDirectory}

import scala.jdk.CollectionConverters._
import scala.util.Using


//TODO could store the document too instead of using a store to retrieve the content. Perhaps that's another implementation
class LuceneDocumentIndex(directory: Path, parser: DocumentParser, store: DocumentStore) extends DocumentIndex with LazyLogging {

  require(Files.isDirectory(directory), s"$directory is not a directory")

  import LuceneDocumentIndex._

  private val index = new Index(FSDirectory.open(directory))

  override def search(query: String): IO[Seq[DocumentMatch]] =
    IO {
      index.search(query, 10)
        .view
        .map(extractId)
        .distinct
        .map(documentId => DocumentMatch(documentId, Seq.empty))
        .toSeq
    }

  override def snippets(query: String): IO[Seq[DocumentMatch]] =
    IO(index.search(query, 10)).flatMap { results =>
      results.toList.map(highlightSnippets).sequence.map(_.toSeq)
    }

  override def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    IO(index.searchInId(documentId.value, query)).flatMap(_.map(highlightSnippets).sequence)

  private def highlightSnippets(result: SearchResult): IO[DocumentMatch] =
    highlightDocument(result, snippetsOnly = true)

  override def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    IO(index.searchInId(documentId.value, query)).flatMap(_.map(highlightWholeDocument).sequence)

  private def highlightWholeDocument(result: SearchResult): IO[DocumentMatch] =
    highlightDocument(result, snippetsOnly = false)

  private def highlightDocument(result: SearchResult, snippetsOnly: Boolean): IO[DocumentMatch] = {
    val documentId = extractId(result)
    store
      .get(documentId)
      .flatMap(parser.parse)
      .map { parsed =>
        val highlighter = createHighlighter(parsed.content, snippetsOnly)

        val fq = highlighter.getFieldQuery(result.q)
        val fragments = highlighter
          .getBestFragments(fq, result.reader, result.docId, fields.content, maxFragments, highlightLimit)
          .toSeq

        DocumentMatch(documentId, fragments)
      }
  }

  //TODO check if id already exists
  override def add(document: Document): IO[Unit] =
    parser.parse(document).map { parsed =>
      val luceneDoc = new LuceneDocument
      //      metadata.foreach { case (field, value) =>
      //        d.add(new TextField(field, value, Store.YES))
      //      }
      luceneDoc.add(new StringField(fields.id, parsed.documentId.value, Store.YES))
      //      d.add(new StringField(timestampField, self.timestamp.toString, Store.YES))
      //      d.add(new StringField(metadataTimestampField, self.metadataTimestamp.toString, Store.YES))
      luceneDoc.add(new StringField(fields.language, parsed.language, Store.YES))
      luceneDoc.add(new TextWithOffsetsField(fields.content, parsed.content))

      index.insert(parsed.language, luceneDoc)
    }

  override def remove(documentId: DocumentId): IO[Unit] =
    IO(index.delete(documentId))

  override def update(updatedDocument: Document): IO[Unit] = ???

  override def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit] = ???
}

object LuceneDocumentIndex {

  private val maxFragments = 1000
  private val highlightLimit = 3
  private val preTag = (i: Int) => s"""<span class="highlight-$i">"""
  private val postTag = (_: Int) => "</span>"
  private val defaultLanguage = "en"
  private val supportedLanguages =
    Seq(
      new IndexLanguage("en", new EnglishAnalyzer()),
      new IndexLanguage("pt", new BrazilianAnalyzer()),
    ).map(indexLanguage => indexLanguage.code -> indexLanguage).toMap
  private val idAnalyzer = new KeywordAnalyzer
  private val idQueryParser =
    queryParser(fields.id, idAnalyzer)

  def extractId(result: SearchResult): DocumentId =
    extractId(result.document)

  def extractId(document: LuceneDocument): DocumentId =
    DocumentId(document.getField(fields.id).stringValue)

  private def createHighlighter(content: String, snippetsOnly: Boolean): FastVectorHighlighter = {
    val fragListBuilder = new SimpleFragListBuilder
    val fragBuilder = TagFragmentBuilder2(content, snippetsOnly, preTag, postTag)
    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
  }

  private def formatQuery(field: String, q: String): String =
    s"$field:($q)"

  private def idQuery(id: String): Query =
    idQueryParser.parse(s"""${fields.id}:"${QueryParserUtil.escape(id)}"""")

  private def queryParser(fieldName: String, analyzer: Analyzer): QueryParser = {
    val parser = new QueryParser(fieldName, analyzer)
    parser.setDefaultOperator(QueryParser.Operator.AND)
    parser
  }

  //TODO just inline this guy?
  private class Index(directory: Directory) extends LazyLogging {

    private val fieldsToLoad = Set(fields.id).asJava

    def insert(language: String, luceneDocument: LuceneDocument): Unit = {
      val analyzer = supportedLanguages.getOrElse(language, supportedLanguages(defaultLanguage)).analyzer
      withWriter(analyzer)(_.addDocument(luceneDocument))
    }

    private def withWriter[T](analyzer: Analyzer)(f: IndexWriter => T): T = {
      val config = new IndexWriterConfig(analyzer).setOpenMode(CREATE_OR_APPEND)
      Using(new IndexWriter(directory, config)) { writer =>
        val result = f(writer)
        writer.commit()
        result
      }
      }.get

    //TODO use ParSeq
    def search(query: String, limit: Int): Seq[SearchResult] =
      try {
        supportedLanguages.values
          .flatMap { language =>
            val q = language.parseContentQuery(query)
            withSearcher { searcher =>
              searcher
                .search(q, limit)
                .scoreDocs
                .map { doc =>
                  language.code -> SearchResult(doc.score, q, searcher.getIndexReader, searcher.doc(doc.doc, fieldsToLoad), doc.doc)
                }
            }
          }
          .groupBy { case (_, result) => result.docId }
          .view
          .values
          .flatMap { results =>
            val (highestScoreLanguage, _) = results.minBy { case (_, result) => -result.score }
            results.toSeq.collect { case (`highestScoreLanguage`, result) => result }
          }
          .toSeq
      } catch {
        case e: Exception =>
          logger.error("Error when searching", e)
          Seq.empty
      }

    private def withSearcher[T](f: IndexSearcher => T): T =
      f(new IndexSearcher(DirectoryReader.open(directory)))

    def searchInId(id: String, query: String): Option[SearchResult] = try {
      supportedLanguages.values
        .toSeq
        .flatMap { language =>
          val q = new BooleanQuery.Builder()
            .add(idQuery(id), BooleanClause.Occur.MUST)
            .add(language.parseContentQuery(query), BooleanClause.Occur.SHOULD)
            .build()
          withSearcher { searcher =>
            searcher
              .search(q, 1)
              .scoreDocs
              .map { d =>
                val doc = searcher.doc(d.doc, fieldsToLoad)
                SearchResult(d.score, q, searcher.getIndexReader, doc, d.doc)
              }
          }
        }
        .sortBy(result => -result.score)
        .headOption
    } catch {
      case e: Exception =>
        logger.error("Error when id searching", e)
        None
    }

    def delete(documentId: DocumentId): Unit =
      withWriter(idAnalyzer)(_.deleteDocuments(idQuery(documentId.value)))
  }

  private class IndexLanguage(val code: String, val analyzer: Analyzer) {

    private val parser: QueryParser = queryParser(fields.content, analyzer)

    def parseQuery(query: String): Query = parser.parse(query)

    def parseContentQuery(query: String): Query = parser.parse(formatQuery(fields.content, query))
  }

  private object fields {
    val id = "id"
    val content = "content"
    val language = "lang"
  }
}