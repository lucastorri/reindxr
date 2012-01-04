package co.torri.reindxr.index

import java.io.File
import java.io.StringReader

import scala.Array.canBuildFrom
import scala.io.Source.fromFile

import org.apache.lucene.analysis.SimpleAnalyzer
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
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


object FilesIndex {

	def indexFrom(indexFolder: File) =
		FilesIndex(IndexFactory(open(indexFolder)))	

}
case class FilesIndex(factory: IndexFactory) {

	private val logger = Logger[FilesIndex]
    private val searchLimit = 20
	private val highlightLimit = 3
    private val identifierField = "uri"
	private val timestampField = "timestamp"
    private val contentField = "contents"
    private val queryParser = factory.newQueryParser(contentField)

	private class FileId(file: File) {
		def id = file.getAbsolutePath
		def timestamp = file.lastModified
	}
	private implicit def file2fileId(file: File) = new FileId(file)
	
	private implicit def writer = factory.newWriter
	
	private def withWriter(exec: IndexWriter => Unit)(implicit writer: IndexWriter, close: Boolean = true) = {
	    exec(writer)
	    if (close) writer.close
	}

	def insert(file: File) : Unit = doInsert(file)
    private def doInsert(file: File)(implicit writer: IndexWriter) : Unit = try withWriter { writer =>
    
		if (factory.indexExists && file.timestamp <= timestampFor(file)) {
			logger.info("Already latest version")
			return
		}
      	doRemove(file)(writer, false)
     	writer.addDocument(file)
    
    } catch { case e => logger.error("Error when indexing", e) }
	
	def remove(file: File) : Unit = doRemove(file)
	private def doRemove(file: File)(implicit writer: IndexWriter, close: Boolean = true) = if (factory.indexExists) try withWriter { writer =>
		
 		writer.deleteDocuments(new Term(identifierField, file.id))
	
	} catch { case e => logger.error("Error when indexing", e) }
	
	private def withSearcher[T](exec: IndexSearcher => T) = {
	    val searcher = factory.newSearcher
	    val ret = exec(searcher)
	    searcher.close
	    ret
	}
	
	private def timestampFor(f: File) : Long = withSearcher { searcher =>
		if (!factory.indexExists) {
			logger.info("File not indexed")
			return 0L
		}
		
		searcher.search(queryParser.parse(timestampField + ":" + f.id), searchLimit)
			.scoreDocs.map(d => searcher.doc(d.doc)).
			headOption.map(_.getFieldable(timestampField).stringValue.toLong).getOrElse(0L)
	}

    def search(query: String): List[(File, List[String])] = try withSearcher { searcher =>
    
		import org.apache.lucene.search.highlight.SimpleHTMLEncoder
	
	    if (!factory.indexExists) {
	        logger.info("Index doesn't exist")
	        return List()
	    }
		val q = queryParser.parse(contentField + ":" + query)
	    val results = searcher.search(q, searchLimit)
		val highlighter = factory.newHighlighter
      	val fq = highlighter.getFieldQuery(q)
	    val files = results.scoreDocs.sortBy(- _.score).map(_.doc).distinct.map{ docId => 
			(
				document2File(searcher.doc(docId)),
				highlighter.getBestFragments(fq, searcher.getIndexReader, docId, contentField, 1000, highlightLimit).toList
			)
		}
		
	    files.toList
    
    } catch { case e => logger.error("Error when searching for " + query, e); List() }
  
    private implicit def document2File(d: Document) : File =
      	new File(d.get(identifierField))
    
    private implicit def file2Document(file: File) : Document = {
      	val doc = new Document
      	doc.add(new Field(identifierField, file.id, Field.Store.YES, Field.Index.NOT_ANALYZED))
		doc.add(new Field(timestampField, file.timestamp.toString, Field.Store.YES, Field.Index.NOT_ANALYZED))
      	doc.add(new Field(contentField, fromFile(file).mkString, Field.Store.YES, Field.Index.ANALYZED, Field.TermVector.WITH_POSITIONS_OFFSETS))
      	doc
    }
	
}

case class IndexFactory(indexPath: Directory) {
  
  	private val version = LUCENE_31
	private val preTag = "<span class=\"highlight\">";
	private val postTag = "</span>";
  
  	def analyzer =
	 	new SimpleAnalyzer(version)

	def config =
    	new IndexWriterConfig(version, analyzer).setOpenMode(CREATE_OR_APPEND)
  
  	def newWriter =
    	new IndexWriter(indexPath, config)
    
  	def newSearcher =
    	new IndexSearcher(indexPath)

    def newQueryParser(fieldName: String) = {
		val parser = new QueryParser(version, fieldName, analyzer)
		parser.setDefaultOperator(QueryParser.AND_OPERATOR)
		parser
	}
	
	def newHighlighter = {
	    val fragListBuilder = new SimpleFragListBuilder
	    val fragBuilder = TagFragmentBuilder(preTag, postTag)
	    new FastVectorHighlighter(true, true, fragListBuilder, fragBuilder)
	}
    	
  
    def indexExists =
    	IndexReader.indexExists(indexPath)
    
}