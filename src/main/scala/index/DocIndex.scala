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
import org.apache.lucene.util.Version.LUCENE_36
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
case class NullDoc(id: String) extends Doc {
	val timestamp = 0L
	val contents = ""
	val language = ""
	val file = null
	val document = null
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
        val m = new MD
        parser.parse(new FileInputStream(f), c, m)
        c.toString
    }
    
    override def contents =
        read(file).toString
}

trait DocLoader extends DocConverter with DocReader { self: Doc => }

case class DocMatch(doc: Doc, matches: Seq[String] = List())

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
    private val snippetHighlighter = config.highlighter(true)
    private val fullDocHighlighter = config.highlighter(false)

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

    def search(query: String) : Seq[DocMatch] =
      	withIndex { index => 
      	  	index.search(query, searchLimit).map{ r => 
      	  		DocMatch(docFactory(r.document))
      	  	}.seq
  	  	}("Error when searching for " + query, List())
        
    def snippets(query: String) : Seq[DocMatch] = 
        withIndex { index =>
            index.search(query, searchLimit).map(snippets(_)).seq
        }("Error when searching for " + query, List())

    def snippets(id: String, query: String) : DocMatch =
      	withIndex { index =>
        	index.searchInId(id, query).map(snippets(_)).getOrElse(DocMatch(NullDoc(id)))
        }("Error when searching for " + query, DocMatch(NullDoc(id)))
        
    private def snippets(r: SearchResult) = {
        val fq = snippetHighlighter.getFieldQuery(r.q)
		DocMatch(
		    docFactory(r.document),
		    snippetHighlighter.getBestFragments(fq, r.reader, r.docId, contentField, maxNumOfFragment, highlightLimit)
		)
    }
        
    def highlight(id: String, query: String) : String = 
        withIndex { index =>
            index.searchInId(id, query).map { r =>
                  val fq = fullDocHighlighter.getFieldQuery(r.q)
                  val hls = fullDocHighlighter.getBestFragments(fq, r.reader, r.docId, contentField, Int.MaxValue, highlightLimit)
                  hls.headOption.getOrElse(docFactory(r.document).contents)
            }.getOrElse("")
        }("Error when highlighting " + id + "with query " + query, "")

    private def withIndex[Ret](exec: IndexAdapter => Ret)(errorMsg: => String, defaultReturn: => Ret = () => null) : Ret = try {
        exec(config.indexAdapter)
    } catch { case e => logger.error(errorMsg, e); defaultReturn }

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
      
    val version = LUCENE_36
  
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
        
        private val logger = Logger[DefaultIndexAdapter]

        def insert(doc: Doc) : Unit = {
            val w = langs(doc.language).writer
            w.addDocument(doc.document)
            w.commit
        }
        
        def search(query: String, limit: Int) : ParSeq[SearchResult] =
            try analyzers.flatMap { a =>
                val q = a.parser.parse(fq(contentField, query))
                a.searcher.search(q, limit).scoreDocs.distinct.map {d =>
                    SearchResult(d.score, q, a.searcher.getIndexReader, a.searcher.doc(d.doc), d.doc)
                }
            }.seq.sortBy(- _.score).par catch { case e => logger.error("Error when searching", e); ParSeq() }
          
        def searchId(id: String) : Option[Document] =
            searchInId(id, id).map(r => r.document)

        def searchInId(id: String, query: String) : Option[SearchResult] = try {
            val q = idQueryParser.parse(fq(idField, id))
                analyzers.flatMap { a =>
                    a.searcher.search(q, 1).scoreDocs.map { d => 
                        val doc = a.searcher.doc(d.doc)
                        val q = a.parser.parse(fq(contentField, query))
                        SearchResult(d.score, q, a.searcher.getIndexReader, doc, d.doc)
                    }
                }.seq.headOption
        } catch { case e => logger.error("Error when id searching", e); None }
              
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
        def searcher = new IndexSearcher(IndexReader.open(writer, true))
        lazy val parser = queryParser(contentField, analyzer)
        
        def indexExists =
            IndexReader.indexExists(dir)
        
        def close = {
            try { searcher.close } catch { case e => }
            try { writer.close } catch { case e => }
        }
        
        def toPair = (lang, this)
    }
}
