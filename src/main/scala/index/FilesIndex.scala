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
import org.apache.lucene.util.Version.LUCENE_31

import grizzled.slf4j.Logger


object FilesIndex {

	def indexFrom(indexFolder: File) =
		FilesIndex(IndexFactory(open(indexFolder)))	

}
case class FilesIndex(factory: IndexFactory) {

	private val logger = Logger[FilesIndex]
    private val searchLimit = 20
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
	
	private def withWriter(exec: IndexWriter => Unit, close: Boolean = true)(implicit writer: IndexWriter) = {
	    exec(writer)
	    if (close) writer.close
	}

	def insert(file: File) : Unit = doInsert(file)
    private def doInsert(file: File)(implicit writer: IndexWriter) : Unit = try withWriter { writer =>
    
		if (factory.indexExists && file.timestamp <= timestampFor(file)) {
			logger.info("Already latest version")
			return
		}
      	doRemove(file, false)(writer)
     	writer.addDocument(file)
    
    } catch { case e => logger.error("Error when indexing", e) }
	
	def remove(file: File) : Unit = doRemove(file)
	private def doRemove(file: File, close: Boolean = true)(implicit writer: IndexWriter) = if (factory.indexExists) try withWriter ({ writer =>
		
 		writer.deleteDocuments(new Term(identifierField, file.id))
	
	}, close) catch { case e => logger.error("Error when indexing", e) }
	
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

    def search(query: String): List[File] = try withSearcher { searcher =>
    
	    if (!factory.indexExists) {
	        logger.info("Index doesn't exist")
	        return List()
	    }
	    val results = searcher.search(queryParser.parse(contentField + ":" + query), searchLimit)
	    val files = results.scoreDocs.map(_.doc).distinct.map(docId => document2File(searcher.doc(docId)))

	    files.distinct.toList
    
    } catch { case e => logger.error("Error when searching for " + query, e); List() }
  
    private implicit def document2File(d: Document) : File =
      	new File(d.get(identifierField))
    
    private implicit def file2Document(file: File) : Document = {
      	val doc = new Document
      	doc.add(new Field(identifierField, file.id, Field.Store.YES, Field.Index.toIndex(true, false)))
		doc.add(new Field(timestampField, file.timestamp.toString, Field.Store.YES, Field.Index.toIndex(true, false)))
      	doc.add(new Field(contentField, new StringReader(fromFile(file).mkString)))
      	doc
    }
	
}

case class IndexFactory(indexPath: Directory) {
  
  	private val version = LUCENE_31
  
  	private def analyzer =
	 	new SimpleAnalyzer(version)

	private def config =
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
    	
  
    def indexExists =
    	IndexReader.indexExists(indexPath)
    
}