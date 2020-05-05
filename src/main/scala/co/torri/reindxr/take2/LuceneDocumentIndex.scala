package co.torri.reindxr.take2

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.implicits._
import co.torri.reindxr.index.{SearchResult, TextWithOffsetsField}
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
import org.apache.lucene.store.FSDirectory

import scala.jdk.CollectionConverters._
import scala.util.Using


//TODO could store the document too instead of using a store to retrieve the content. Perhaps that's another implementation
class LuceneDocumentIndex(directory: Path, parser: DocumentParser, store: DocumentStore) extends DocumentIndex {

  require(Files.isDirectory(directory), s"$directory is not a directory")

  import LuceneDocumentIndex._

  private val fieldsToLoad = Set(fields.id).asJava
  private val luceneDirectory = FSDirectory.open(directory)

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

      val analyzer = supportedLanguages.getOrElse(parsed.language, supportedLanguages(defaultLanguage)).analyzer
      withWriter(analyzer)(_.addDocument(luceneDoc))
    }

  override def search(query: String): IO[Seq[DocumentMatch]] =
    IO {
      search(query, 10)
        .view
        .map(extractId)
        .distinct
        .map(documentId => DocumentMatch(documentId, Seq.empty))
        .toSeq
    }

  override def snippets(query: String): IO[Seq[DocumentMatch]] =
    IO(search(query, 10)).flatMap { results =>
      results.toList.map(highlightSnippets).sequence.map(_.toSeq)
    }

  override def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    IO(searchInId(documentId.value, query)).flatMap(_.map(highlightSnippets).sequence)

  override def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    IO(searchInId(documentId.value, query)).flatMap(_.map(highlightWholeDocument).sequence)

  override def remove(documentId: DocumentId): IO[Unit] =
    withWriter(idAnalyzer)(_.deleteDocuments(idQuery(documentId.value)))

  override def update(updatedDocument: Document): IO[Unit] = ???

  override def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit] = ???

  //TODO use ParSeq
  private def search(query: String, limit: Int): Seq[SearchResult] =
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

  private def highlightWholeDocument(result: SearchResult): IO[DocumentMatch] =
    highlightDocument(result, snippetsOnly = false)

  private def highlightSnippets(result: SearchResult): IO[DocumentMatch] =
    highlightDocument(result, snippetsOnly = true)

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

  private def searchInId(id: String, query: String): Option[SearchResult] =
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

  private def withSearcher[T](f: IndexSearcher => T): T =
    f(new IndexSearcher(DirectoryReader.open(luceneDirectory)))

  private def withWriter[T](analyzer: Analyzer)(f: IndexWriter => T): IO[T] = IO.fromTry {
    val config = new IndexWriterConfig(analyzer).setOpenMode(CREATE_OR_APPEND)
    Using(new IndexWriter(luceneDirectory, config)) { writer =>
      val result = f(writer)
      writer.commit()
      result
    }
  }
}

object LuceneDocumentIndex {

  private val maxFragments = 1000
  private val highlightLimit = 3
  private val preTag = (i: Int) => s"""<span class="highlight-$i">"""
  private val postTag = (_: Int) => "</span>"
  private val idAnalyzer = new KeywordAnalyzer
  private val idQueryParser = createQueryParser(fields.id, idAnalyzer)
  private val defaultLanguage = "en"
  private val supportedLanguages =
    Seq(
      new IndexLanguage("en", new EnglishAnalyzer()),
      new IndexLanguage("pt", new BrazilianAnalyzer()),
    ).map(indexLanguage => indexLanguage.code -> indexLanguage).toMap

  private def idQuery(id: String): Query =
    idQueryParser.parse(s"""${fields.id}:"${QueryParserUtil.escape(id)}"""")

  private def extractId(result: SearchResult): DocumentId =
    extractId(result.document)

  private def extractId(document: LuceneDocument): DocumentId =
    DocumentId(document.getField(fields.id).stringValue)

  private def createHighlighter(content: String, snippetsOnly: Boolean): FastVectorHighlighter = {
    val fragListBuilder = new SimpleFragListBuilder
    val fragBuilder = TagFragmentBuilder2(content, snippetsOnly, preTag, postTag)
    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
  }

  private def createQueryParser(fieldName: String, analyzer: Analyzer): QueryParser = {
    val parser = new QueryParser(fieldName, analyzer)
    parser.setDefaultOperator(QueryParser.Operator.AND)
    parser
  }

  private class IndexLanguage(val code: String, val analyzer: Analyzer) {

    private val parser: QueryParser = createQueryParser(fields.content, analyzer)

    def parseQuery(query: String): Query = parser.parse(query)

    def parseContentQuery(query: String): Query = parser.parse(s"${fields.content}:($query)")
  }

  private object fields {
    val id = "id"
    val content = "content"
    val language = "lang"
  }

}
