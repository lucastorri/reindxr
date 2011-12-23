package co.torri.reindxr.index

import java.io._
import scala.io.Source._
import grizzled.slf4j._
import org.apache.lucene.analysis._
import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.store._
import org.apache.lucene.util.Version._
import org.apache.lucene.queryParser._
import org.apache.lucene.search._
import org.apache.lucene.store.FSDirectory._
import org.apache.lucene.index.IndexWriterConfig.OpenMode._


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

    def insert(file: File) : Unit = try {
    
		if (file.timestamp <= timestampFor(file)) {
			logger.warn("Already latest version")
			return
		}
	
      	val writer = factory.newWriter
      	if (!factory.indexExists) {
     		writer.deleteDocuments(new Term(identifierField, file.id))
      	}
     	writer.addDocument(file)
      	writer.close
    
    } catch { case e => logger.error("Error when indexing", e) }
	
	private def timestampFor(f: File) : Long = {
		if (!factory.indexExists) {
			logger.warn("File not indexed")
			return 0L
		}
		
		val searcher = factory.newSearcher
		val timestamp = searcher.search(queryParser.parse(timestampField + ":" + f.id), searchLimit)
			.scoreDocs.map(d => searcher.doc(d.doc)).
			firstOption.map(_.getFieldable(timestampField).stringValue.toLong).getOrElse(0L)
		searcher.close
		timestamp
	}
	
	def remove(file: File) = try {
		
      	val writer = factory.newWriter
      	if (!factory.indexExists) {
     		writer.deleteDocuments(new Term(identifierField, file.id))
      	}
      	writer.close
	
	} catch { case e => logger.error("Error when indexing", e) }

    def search(query: String): List[File] = try {
    
	    if (!factory.indexExists) {
	        logger.warn("Index doesn't exist")
	        return List()
	    }
	    val searcher = factory.newSearcher
	    val results = searcher.search(queryParser.parse(contentField + ":" + query), searchLimit)
	    val files = results.scoreDocs.map(_.doc).distinct.map(docId => document2File(searcher.doc(docId)))
	    searcher.close

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
	 	new StandardAnalyzer(version)

	private def config =
    	new IndexWriterConfig(version, analyzer).setOpenMode(CREATE_OR_APPEND)
  
  	def newWriter =
    	new IndexWriter(indexPath, config)
    
  	def newSearcher =
    	new IndexSearcher(indexPath)

    def newQueryParser(fieldName: String) =
    	new QueryParser(version, fieldName, analyzer)
  
    def indexExists =
    	IndexReader.indexExists(indexPath)
    
}