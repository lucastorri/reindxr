package co.torri.reindxr.take2

import java.io.{InputStream, OutputStream}
import java.lang.{StringBuilder => JavaStringBuilder}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.Properties

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
import org.apache.lucene.search.vectorhighlight.{FastVectorHighlighter, FieldFragList, SimpleFragListBuilder, SimpleFragmentsBuilder}
import org.apache.lucene.search.{BooleanClause, BooleanQuery, IndexSearcher, Query}
import org.apache.lucene.store.{Directory, FSDirectory}
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.language.LanguageIdentifier
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.BodyContentHandler

import scala.jdk.CollectionConverters._
import scala.util.Using


trait Take2

sealed abstract case class AccountId(value: String)

object AccountId {
  def apply(id: String): AccountId = new AccountId(id) {}
}

sealed abstract case class DocumentId(value: String)

object DocumentId {
  def apply(id: String): DocumentId = new DocumentId(id) {}
}

trait Document {
  def id: DocumentId

  def content: IO[InputStream]

  def metadata: Map[String, String]
}

trait Reindxr {
  def add(document: Document): IO[Unit]

  def search(query: String): IO[Seq[DocumentMatch]]

  def snippets(query: String): IO[Seq[DocumentMatch]]

  def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]]

  def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]]

  def remove(documentId: DocumentId): IO[Unit]

  def update(updatedDocument: Document): IO[Unit]

  def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit]
}

class StoreIndexSplitReindxr(store: DocumentStore, index: DocumentIndex) extends Reindxr {

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

trait DocumentStore {
  def add(document: Document): IO[Unit]

  def get(documentId: DocumentId): IO[Document]

  def remove(documentId: DocumentId): IO[Unit]

  def update(updatedDocument: Document): IO[Unit]

  def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit]
}

trait DocumentIndex {

  def add(document: Document): IO[Unit]

  def update(updatedDocument: Document): IO[Unit]

  def remove(documentId: DocumentId): IO[Unit]

  def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit]

  def search(query: String): IO[Seq[DocumentMatch]]

  def snippets(query: String): IO[Seq[DocumentMatch]]

  def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]]

  def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]]

}

case class DocumentMatch(documentId: DocumentId, fragments: Seq[String])

case class ParsedDocument(
                           documentId: DocumentId,
                           language: String,
                           content: String,
                           contentMetadata: Map[String, String],
                           userMetadata: Map[String, String],
                         )

trait DocumentParser {
  def parse(document: Document): IO[ParsedDocument]
}

object Main {
  def main(args: Array[String]): Unit = {
    val documentsDirectory: Path = Paths.get("/Users/lucastorri/tmp/docs")
    val indexDirectory: Path = Paths.get("/Users/lucastorri/tmp/index")
    val parser: DocumentParser = new TikaDocumentParser

    def store(accountId: AccountId): DocumentStore =
      new FilesystemDocumentStore(Files.createDirectories(documentsDirectory.resolve(accountId.value)))

    def index(accountId: AccountId): DocumentIndex =
      new LuceneDocumentIndex(Files.createDirectories(indexDirectory.resolve(accountId.value)), parser, store(accountId))

    def reindxr(accountId: AccountId): Reindxr =
      new StoreIndexSplitReindxr(store(accountId), index(accountId))

    val default = reindxr(AccountId("default"))

//    default.add(new Document {
//      override def id: DocumentId = DocumentId("hi!")
//
//      override def content: IO[InputStream] = IO(Files.newInputStream(Paths.get("/Users/lucastorri/Downloads/WHO-COVID-19-Community_Transmission-2020.1-eng.pdf")))
//
//      override def metadata: Map[String, String] = Map(
//        "id" -> "hi!",
//        "length" -> "big",
//        "timestamp" -> "long ago",
//      )
//    }).unsafeRunSync()

    println("===============")
    default.search("WHO").unsafeRunSync().foreach(println)
//    default.search("length:big WHO").unsafeRunSync().foreach(println)
    println("---------------")
    default.snippets("outbreak").unsafeRunSync().foreach(println)
    default.snippets(DocumentId("hi!"), "outbreak").unsafeRunSync().foreach(println)
    default.highlight(DocumentId("hi!"), "outbreak").unsafeRunSync().foreach(println)
  }
}

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
  //TODO snippets seem broken
    IO(index.search(query, 10)).flatMap { results =>
      results.toList.map(highlightSnippets).sequence.map(_.toSeq)
    }

  override def snippets(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    IO(index.searchInId(documentId.value, query)).flatMap(_.map(highlightSnippets).sequence)

  override def highlight(documentId: DocumentId, query: String): IO[Option[DocumentMatch]] =
    IO(index.searchInId(documentId.value, query)).flatMap(_.map(highlightWholeDocument).sequence)

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

  private def highlightSnippets(result: SearchResult): IO[DocumentMatch] =
    highlightDocument(result, snippetsOnly = true)

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
}

object LuceneDocumentIndex {

  private val maxFragments = 1000
  private val highlightLimit = 3

  private object fields {
    val id = "id"
    val content = "content"
    val language = "lang"
  }

  private val preTag = (i: Int) => s"""<span class="highlight-$i">"""
  private val postTag = (_: Int) => "</span>"

  private def createHighlighter(content: String, snippetsOnly: Boolean): FastVectorHighlighter = {
    val fragListBuilder = new SimpleFragListBuilder
    val fragBuilder = TagFragmentBuilder2(content, snippetsOnly, preTag, postTag)
    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
  }

  def extractId(result: SearchResult): DocumentId =
    extractId(result.document)

  def extractId(document: LuceneDocument): DocumentId =
    DocumentId(document.getField(fields.id).stringValue)

  private val defaultLanguage = "en"
  private val supportedLanguages =
    Seq(
      new IndexLanguage("en", new EnglishAnalyzer()),
      new IndexLanguage("pt", new BrazilianAnalyzer()),
    ).map(indexLanguage => indexLanguage.code -> indexLanguage).toMap

  private def formatQuery(field: String, q: String): String =
    s"$field:($q)"

  private val idAnalyzer = new KeywordAnalyzer

  //TODO just inline this guy?
  private class Index(directory: Directory) extends LazyLogging {

    private val fieldsToLoad = Set(fields.id).asJava

    def insert(language: String, luceneDocument: LuceneDocument): Unit = {
      val analyzer = supportedLanguages.getOrElse(language, supportedLanguages(defaultLanguage)).analyzer
      withWriter(analyzer)(_.addDocument(luceneDocument))
    }

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

    private def withSearcher[T](f: IndexSearcher => T): T =
      f(new IndexSearcher(DirectoryReader.open(directory)))

    private def withWriter[T](analyzer: Analyzer)(f: IndexWriter => T): T = {
      val config = new IndexWriterConfig(analyzer).setOpenMode(CREATE_OR_APPEND)
      Using(new IndexWriter(directory, config)) { writer =>
        val result = f(writer)
        writer.commit()
        result
      }
      }.get
  }

  private def idQuery(id: String): Query =
    idQueryParser.parse(s"""${fields.id}:"${QueryParserUtil.escape(id)}"""")

  private class IndexLanguage(val code: String, val analyzer: Analyzer) {

    private val parser: QueryParser = queryParser(fields.content, analyzer)

    def parseQuery(query: String): Query = parser.parse(query)

    def parseContentQuery(query: String): Query = parser.parse(formatQuery(fields.content, query))
  }

  private def queryParser(fieldName: String, analyzer: Analyzer): QueryParser = {
    val parser = new QueryParser(fieldName, analyzer)
    parser.setDefaultOperator(QueryParser.Operator.AND)
    parser
  }

  private val idQueryParser =
    queryParser(fields.id, idAnalyzer)
}

class TikaDocumentParser extends DocumentParser {
  private val parser = new AutoDetectParser(new DefaultDetector)

  override def parse(document: Document): IO[ParsedDocument] =
    document.content.map { in =>
      val contentHandler = new BodyContentHandler(Int.MaxValue)
      val metadata = new Metadata
      parser.parse(in, contentHandler, metadata)
      val content = contentHandler.toString
      val language = new LanguageIdentifier(content).getLanguage

      ParsedDocument(
        document.id,
        language,
        content,
        metadata.names().map(name => name -> metadata.get(name)).toMap,
        document.metadata,
      )
    }
}

class FilesystemDocumentStore(directory: Path) extends DocumentStore {

  require(Files.isDirectory(directory), s"$directory is not a directory")

  override def add(document: Document): IO[Unit] =
    for {
      contentIn <- document.content
      _ <- contentFile(document.id).out(contentIn.transferTo)
      _ <- updateMetadata(document.id, document.metadata)
    } yield ()

  override def get(documentId: DocumentId): IO[Document] =
    for {
      metadata <- metadataFile(documentId).in(propertiesToMetadata)
    } yield new FileDocument(documentId, metadata)

  override def remove(documentId: DocumentId): IO[Unit] =
    for {
      _ <- contentFile(documentId).delete()
      _ <- metadataFile(documentId).delete()
    } yield ()

  override def update(updatedDocument: Document): IO[Unit] =
    add(updatedDocument)

  override def updateMetadata(documentId: DocumentId, newMetadata: Map[String, String]): IO[Unit] =
    metadataFile(documentId).out(metadataToProperties(newMetadata).store(_, documentId.value))

  private def contentFile(documentId: DocumentId): InternalFile =
    new InternalFile(documentId, "")

  private def metadataFile(documentId: DocumentId): InternalFile =
    new InternalFile(documentId, ".metadata")

  private def metadataToProperties(metadata: Map[String, String]): Properties = {
    val properties = new Properties()
    properties.putAll(metadata.asJava)
    properties
  }

  private def propertiesToMetadata(in: InputStream): Map[String, String] = {
    val properties = new Properties()
    properties.load(in)
    properties.asScala.toMap
  }

  private class InternalFile(documentId: DocumentId, extension: String) {
    private val path = directory.resolve(s"${idHash(documentId)}$extension")

    def in[A]: InputStream = Files.newInputStream(path)

    def in[A](f: InputStream => A): IO[A] = IO.fromTry(Using(in)(f))

    def out[A](f: OutputStream => A): IO[A] = IO.fromTry(Using(Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING))(f))

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

case class TagFragmentBuilder2(content: String, snippetsOnly: Boolean, preTag: Int => String, postTag: Int => String) extends SimpleFragmentsBuilder(Array(preTag(0)), Array(postTag(0))) {

  private val tagsUsualSize: Int = preTag(0).length + postTag(0).length

  override def createFragments(reader: IndexReader, docId: Int, fieldName: String, fieldFragList: FieldFragList, maxNumFragments: Int): Array[String] = {
    val terms = extractTerms(fieldFragList)
    if (terms.isEmpty) {
      Array.empty
    } else {
      val fragments = terms.groupBy(_.line)
        .toSeq
        .sortBy { case (_, terms) => -terms.size }
        .take(maxNumFragments)
        .map { case (line, terms) =>

          val snippetSize = line.size + (terms.size * tagsUsualSize)
          val buf = new JavaStringBuilder(snippetSize)

          val lastAppended = terms
            .sortBy(_.start)
            .foldLeft(line.start) { case (lastAppended, term) =>
              buf
                .append(content, lastAppended, term.start)
                .append(preTag(term.number))
                .append(content, term.start, term.end)
                .append(postTag(term.number))

              term.end
            }

          buf.append(content, lastAppended, line.end).toString
        }
        .toArray

      if (snippetsOnly && fragments.length < maxNumFragments) {
        val b = fragments.toBuffer
        content.linesWithSeparators.toList.takeWhile { l =>
          if (!l.trim.isEmpty && !b.contains(l)) {
            b += l
          }
          b.size < maxNumFragments
        }
        b.toArray
      } else if (!snippetsOnly && fragments.isEmpty) {
        Array(content)
      } else {
        fragments
      }
    }
  }

  private def extractTerms(fieldFragList: FieldFragList): Seq[MatchedTerm] = {
    if (content.isEmpty) {
      return Seq.empty
    }

    val innerTerms = for {
      fragInfo <- fieldFragList.getFragInfos.asScala
      subInfo <- fragInfo.getSubInfos.asScala
      termOffset <- subInfo.getTermsOffsets.asScala
    } yield MatchedTerm(content, termOffset.getStartOffset, termOffset.getEndOffset, subInfo.getSeqnum)

    innerTerms.toSeq
  }

  case class MatchedTerm(source: String, start: Int, end: Int, number: Int) {

    val stop = '\n'

    val line: Line = {
      val startFragPosition =
        if (snippetsOnly) Some(source.lastIndexOf(stop, start)).filter(_ >= 0).map(_ + 1).getOrElse(0)
        else 0
      val endFragPosition =
        if (snippetsOnly) Some(source.indexOf(stop, end)).filter(_ >= 0).getOrElse(source.length)
        else source.length
      Line(source, startFragPosition, endFragPosition)
    }
  }

  case class Line(source: String, start: Int, end: Int) {

    lazy val content: String = source.substring(start, end)

    def size: Int = end - start
  }

}
