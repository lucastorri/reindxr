package co.torri.reindxr.index

import java.io.File.{separator => |}
import java.io.File
import java.io.FileInputStream

import scala.Array.canBuildFrom
import scala.annotation.implicitNotFound

import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.KeywordAnalyzer
import org.apache.lucene.document.Field.Index
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.Field.TermVector
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.index.IndexWriterConfig.OpenMode.CREATE_OR_APPEND
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.index.Term
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.search.vectorhighlight.FastVectorHighlighter
import org.apache.lucene.search.vectorhighlight.SimpleFragListBuilder
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.store.FSDirectory.open
import org.apache.lucene.store.Directory
import org.apache.lucene.util.Version.LUCENE_31
import org.apache.tika.detect.DefaultDetector
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.AutoDetectParser
import org.apache.tika.sax.BodyContentHandler

import DocFields.contentField
import DocFields.identifierField
import DocFields.timestampField
import grizzled.slf4j.Logger


trait DocFields {
	val identifierField = "id"
    val timestampField = "timestamp"
    val contentField = "contents"
}
object DocFields extends DocFields

trait Doc {
	def id : String
	def timestamp : Long
	def contents : String
	
	def file : File
	def document : Document
}
object Doc {
	private implicit def file2string(f: File) = f.getCanonicalPath
	def apply(basepath: File, f: File) = FileDoc(basepath, f)
    def apply(basepath: File, d: Document) = DocumentDoc(basepath, d) 
    def factory(basepath: File) : DocFactory = new DocFactory {
    	def apply(f: File) = Doc(basepath, f)
    	def apply(d: Document) = Doc(basepath, d) 
    }
}

trait DocFactory {
	def apply(f: File) : Doc
    def apply(d: Document) : Doc
}

case class FileDoc(basepath: String, file: File) extends Doc with DocLoader {
	
	lazy val id = {
		val id = file.getAbsolutePath.replace(basepath, "")
    	if (id.startsWith(|)) id.replace(|, "") else id
  	}
  	
  	def timestamp = file.lastModified
}

case class DocumentDoc(basepath: String, d: Document) extends Doc with DocLoader {
  
	lazy val id = d.getFieldable(identifierField).stringValue
  
	lazy val timestamp = d.getFieldable(timestampField).stringValue.toLong
	
	def file = new File(basepath + | + id)
}

trait DocConverter extends DocFields { self : Doc =>
  
	def document = {
  		val d = new Document
        d.add(new Field(identifierField, self.id, Store.YES, Index.NOT_ANALYZED))
        d.add(new Field(timestampField, self.timestamp.toString, Store.YES, Index.NOT_ANALYZED))
        d.add(new Field(contentField, self.contents, Store.NO, Index.ANALYZED, TermVector.WITH_POSITIONS_OFFSETS))
        d
  	}
  
}

trait DocReader { self : Doc =>
	private val parser = new AutoDetectParser(new DefaultDetector)
	
	private def read(f: File) = {
		val c = new BodyContentHandler(Int.MaxValue)
		val m = new Metadata
		parser.parse(new FileInputStream(f), c, m)
		c.toString
	}
	
	override def contents = read(file).toString
}

trait DocLoader extends DocConverter with DocReader { self: Doc => }

case class DocMatch(doc: Doc, matches: Seq[String])

object DocIndex {

    private val preTag = "<span class=\"highlight"+(_:Int)+"\">";
    private val postTag = (i: Int) => "</span>";
  
    def apply(indexFolder: File, basepath: File) : DocIndex =
    	DocIndex(DocIndexConfig(open(indexFolder), basepath, preTag, postTag))

}
case class DocIndex(factory: DocIndexConfig, searchLimit: Int = 20, highlightLimit: Int = 3, maxNumOfFragment: Int = 1000) {

	import DocFields._
    private val logger = Logger[DocIndex]
    private val queryParser = factory.newQueryParser(contentField)
    private val idQueryParser = factory.newIdQueryParser(identifierField)
    private val docFactory = factory.newDocFactory
    private implicit lazy val writer = factory.newWriter

    def insert(doc: Doc) : Unit = 
        if (doc.timestamp > timestampFor(doc)) withWriter { writer =>
            remove(doc)
            writer.addDocument(doc.document)
            logger.info("inserting " + doc.id)
        }("Error when indexing " + doc)

    def remove(doc: Doc) : Unit = 
        withWriter { _.deleteDocuments(new Term(identifierField, doc.id)) }("Error when deleting on " + doc)

    private def timestampFor(doc: Doc) : Long = 
        withSearcher { searcher =>
            searcher.search(queryParser.parse(identifierField + ":" + doc.id), searchLimit)
                .scoreDocs.map(d => searcher.doc(d.doc))
                .headOption.map(_.getFieldable(timestampField).stringValue.toLong).getOrElse(0L)
        }("File not indexed", 0L)

    def search(query: String): List[DocMatch] = 
        withSearcher { searcher =>

            val q = queryParser.parse(contentField + ":" + query)
            val results = searcher.search(q, searchLimit)
            val highlighter = factory.newHighlighter(true)
            val fq = highlighter.getFieldQuery(q)
            val files = results.scoreDocs.sortBy(- _.score).map(_.doc).distinct.par.map{ docId =>
                DocMatch(
                    docFactory(searcher.doc(docId)),
                    highlighter.getBestFragments(fq, searcher.getIndexReader, docId, contentField, maxNumOfFragment, highlightLimit)
                )
            }

            files.toList

        }("Error when searching for " + query, List())

    def highlight(query: String, id: String) : String = 
        withSearcher { searcher =>

            val q = idQueryParser.parse(identifierField + ":" + id)
            val results = searcher.search(q, searchLimit)
            val highlighter = factory.newHighlighter(false)

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

        }("Error when highlighting " + id + "with query " + query, "")

    private def withWriter(exec: IndexWriter => Unit)(errorMsg: => String)(implicit writer: IndexWriter) = try {
        exec(writer)
        writer.commit
    } catch { case e => logger.error(errorMsg, e) }

    private def withSearcher[T](exec: IndexSearcher => T)(errorMsg: => String, defaultReturn: => T) : T =  try {
        val searcher = factory.newSearcher
        val ret = exec(searcher)
        searcher.close
        ret
    } catch { case e => logger.error(errorMsg, e); defaultReturn }

    def close = {
        writer.close
        factory.close
    }

}

case class DocIndexConfig(indexpath: Directory, basepath: File, preTag: Int => String, postTag: Int => String) {

    private val version = LUCENE_31

    def newAnalyzer =
        new EnglishAnalyzer(version)

    def newKeywordAnalyzer =
        new KeywordAnalyzer

    def config =
        new IndexWriterConfig(version, newAnalyzer).setOpenMode(CREATE_OR_APPEND)

    def newWriter =
        new IndexWriter(indexpath, config)

    def newSearcher =
        new IndexSearcher(indexpath)

    def newQueryParser(fieldName: String, analyzer: Analyzer = newAnalyzer) = {
        val parser = new QueryParser(version, fieldName, analyzer)
        parser.setDefaultOperator(QueryParser.AND_OPERATOR)
        parser
    }
    
    def newIdQueryParser(fieldName: String) =
        newQueryParser(fieldName, newKeywordAnalyzer)

    def newHighlighter(snippetsOnly: Boolean) = {
        val fragListBuilder = new SimpleFragListBuilder
        val fragBuilder = TagFragmentBuilder(newDocFactory, snippetsOnly, preTag, postTag)
        new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
    }
    
    def newDocFactory = 
      Doc.factory(basepath)

    def indexExists =
        IndexReader.indexExists(indexpath)

    def close =
        indexpath.close
}
