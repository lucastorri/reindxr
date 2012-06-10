package co.torri.reindxr.index

import java.io.File.{separator => |}
import java.io.File
import java.io.FileInputStream
import scala.Array.canBuildFrom
import org.apache.lucene.analysis.br.BrazilianAnalyzer
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
import org.apache.tika.language.LanguageIdentifier
import scala.collection.GenSeq
import org.apache.lucene.search.Query
import scala.collection.parallel.ParSeq


trait DocFields {
	val identifierField = "id"
    val timestampField = "timestamp"
    val contentField = "contents"
    val languageField = "lang"
}
object DocFields extends DocFields

trait Doc {
	def id : String
	def timestamp : Long
	def contents : String
	def language : String
	
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
  	
  	lazy val language =
		new LanguageIdentifier(contents).getLanguage
}

case class DocumentDoc(basepath: String, d: Document) extends Doc with DocLoader {
  
	lazy val id = d.getFieldable(identifierField).stringValue
  
	lazy val timestamp = d.getFieldable(timestampField).stringValue.toLong
	
	def file = new File(basepath + | + id)
	
	def language = d.getFieldable(languageField).stringValue
}

trait DocConverter extends DocFields { self : Doc =>
  
	def document = {
  		val d = new Document
        d.add(new Field(identifierField, self.id, Store.YES, Index.NOT_ANALYZED))
        d.add(new Field(timestampField, self.timestamp.toString, Store.YES, Index.NOT_ANALYZED))
        d.add(new Field(languageField, self.language, Store.YES, Index.NOT_ANALYZED))
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
    	DocIndex(DocIndexConfig(indexFolder, basepath, preTag, postTag, DocFields.identifierField, DocFields.contentField))

}
case class DocIndex(config: DocIndexConfig, searchLimit: Int = 20, highlightLimit: Int = 3, maxNumOfFragment: Int = 1000) {

	import DocFields._
    private val logger = Logger[DocIndex]
    private val docFactory = config.docFactory

    def insert(doc: Doc) : Unit = 
        if (doc.timestamp > timestampFor(doc)) withIndex { index =>
            index.delete(doc)
            index.insert(doc)
            logger.info("inserting " + doc.id + " [" + doc.language + "]")
        }("Error when inserting " + doc.id)

    def remove(doc: Doc) : Unit = 
        withIndex { index =>
        	index.delete(doc) 
        	logger.info("removing " + doc.id)
        }("Error when deleting " + doc.id)

    private def timestampFor(doc: Doc) : Long = 
        withIndex { index =>
          	index.searchId(doc.id).map(_.getFieldable(timestampField).stringValue.toLong).getOrElse(0L)
        }("File not indexed " + doc.id, 0L)

    def search(query: String): Seq[DocMatch] = 
        withIndex { index =>

            val highlighter = config.highlighter(true)
            index.search(query, searchLimit).map { r =>
              	val fq = highlighter.getFieldQuery(r.q)
            	DocMatch(
            	    docFactory(r.document),
            	    highlighter.getBestFragments(fq, r.reader, r.docId, contentField, maxNumOfFragment, highlightLimit)
            	)
            }

        }("Error when searching for " + query, List())

    def highlight(query: String, id: String) : String = 
        withIndex { index =>

            val hl = config.highlighter(false)
            val result = index.searchInId(id, query)
            
            result.map { r =>
              	val d = docFactory(r.document)
              	val fq = hl.getFieldQuery(r.q)
              	val hls = hl.getBestFragments(fq, r.reader, r.docId, contentField, Int.MaxValue, highlightLimit)
              	hls.headOption.getOrElse(d.contents)
            }.getOrElse("")

        }("Error when highlighting " + id + "with query " + query, "")

    private def withIndex[Ret](exec: IndexAdapter => Ret)(errorMsg: => String, defaultReturn: => Ret = () => null) : Ret = try {
        exec(config.indexAdapter)
    } catch { case e => logger.error(errorMsg, e); defaultReturn }

    def close = {
        config.close
    }

}

trait IndexAdapter {
	def insert(doc: Doc) : Unit
  
	def search(query: String, limit: Int) : Seq[SearchResult]
	
  	def searchId(id: String) : Option[Document]
	
	def searchInId(id: String, query: String) : Option[SearchResult]
	
	def delete(doc: Doc) : Unit
	  	
	def close : Unit
}

case class SearchResult(score: Float, q: Query, reader: IndexReader, document: Document, docId: Int)

case class DocIndexConfig(indexpath: File, basepath: File, preTag: Int => String, postTag: Int => String, idField: String, contentField: String) {
  	
	val version = LUCENE_31
  
	private val idQueryParser = 
		queryParser(idField, new KeywordAnalyzer)
	
	private def fq(field: String, q: String) =
	  	"%s:%s".format(field, q)
	
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
        parser.setDefaultOperator(QueryParser.AND_OPERATOR)
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
  		def insert(doc: Doc) : Unit = {
  			val w = langs(doc.language).writer
  			w.addDocument(doc.document)
  			w.commit
  		}
  	  
  		def search(query: String, limit: Int) : Seq[SearchResult] =
  			analyzers.flatMap { a =>
  			  	val q = a.parser.parse(fq(contentField, query))
  			  	a.searcher.search(q, limit).scoreDocs.distinct.map {d =>
  			  	  	SearchResult(d.score, q, a.searcher.getIndexReader, a.searcher.doc(d.doc), d.doc)
  			  	} 
  			}.seq.sortBy(- _.score)
  		
		def searchId(id: String) : Option[Document] = {
  			  searchInId(id, id).map(r => r.document)
  		}
  			
	  	def searchInId(id: String, query: String) : Option[SearchResult] = {
			val q = idQueryParser.parse(fq(idField, id))
  			analyzers.flatMap { a =>
  			  	a.searcher.search(q, 1).scoreDocs.map { d => 
  			  		val doc = a.searcher.doc(d.doc)
  			  		val q = a.parser.parse(fq(contentField, query))
  			  		SearchResult(d.score, q, a.searcher.getIndexReader, doc, d.doc)
  			  	}
  			}.seq.headOption
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
    		dir
    		new LangDocIndexConfig(lang, analyzer, dir, open(dir))
    	}
    }
    class LangDocIndexConfig(val lang: String, val analyzer: Analyzer, val dir: File, idir: Directory) {
    	
    	lazy val parser = queryParser(contentField, analyzer)
  		lazy val writer = new IndexWriter(idir, config)
  		lazy val searcher = new IndexSearcher(idir)
    	
	  	val config =
	  		new IndexWriterConfig(version, analyzer).setOpenMode(CREATE_OR_APPEND)
  		
  		def close = {
    		searcher.close
    		writer.close
    	}
    	
    	def toPair = (lang, this)
  	}
}
