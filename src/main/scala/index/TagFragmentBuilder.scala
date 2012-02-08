package co.torri.reindxr.index

import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.LowerCaseFilter
import org.apache.lucene.analysis.PorterStemFilter
import org.apache.lucene.analysis.StopFilter
import org.apache.lucene.analysis.TokenFilter
import org.apache.lucene.analysis.TokenStream
import org.apache.lucene.analysis.Tokenizer
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.Field.Index
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.Field.TermVector
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.queryParser.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.Query
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.vectorhighlight.FastVectorHighlighter
import org.apache.lucene.search.vectorhighlight.FieldFragList
import org.apache.lucene.search.vectorhighlight.FieldQuery
import org.apache.lucene.search.vectorhighlight.FragListBuilder
import org.apache.lucene.search.vectorhighlight.FragmentsBuilder
import org.apache.lucene.search.vectorhighlight.SimpleFragListBuilder
import org.apache.lucene.search.vectorhighlight.SimpleFragmentsBuilder
import org.apache.lucene.search.vectorhighlight.FieldFragList.WeightedFragInfo
import org.apache.lucene.search.vectorhighlight.FieldFragList.WeightedFragInfo.SubInfo
import org.apache.lucene.search.vectorhighlight.FieldPhraseList.WeightedPhraseInfo.Toffs
import org.apache.lucene.store.Directory
import org.apache.lucene.store.Directory
import org.apache.lucene.util.Version

import scala.collection.JavaConversions._


case class TagFragmentBuilder(snippetsOnly: Boolean, preTag: Int => String, postTag: Int => String) extends SimpleFragmentsBuilder(Array(preTag(0)), Array(postTag(0))) {
	
	private val maxFragmentsPerFile = 3
		
	override def createFragments(reader: IndexReader, docId: Int, fieldName: String, fieldFragList: FieldFragList, maxNumFragments: Int) : Array[String] = {
		
		val source = reader.document(docId).get(fieldName)
		if (Option(source).map(_.isEmpty).getOrElse(true)) {
			return Array[String]()
		}
		
        val termPositions = 
			for {
				fragInfo <- fieldFragList.getFragInfos
				subInfo <- fragInfo.getSubInfos
				termOffset <- subInfo.getTermsOffsets
			} yield (termOffset.getStartOffset, termOffset.getEndOffset, subInfo.getSeqnum)
		
		val sourceChars = source.toCharArray
			
		val fragments = termPositions.groupBy { case (termStart, termEnd, seqnum) =>
			val startFragPosition = 
				if (snippetsOnly) Some(sourceChars.lastIndexOf('\n', termStart)).map(index => if (index < 0) 0 else index + 1).get
				else 0
			val endFragPosition = 
				if (snippetsOnly) Some(sourceChars.indexOf('\n', termEnd)).map(index => if (index < 0) source.size else index).get
				else source.size
			(startFragPosition, endFragPosition)
		}.toList.sortBy(- _._2.size).take(maxFragmentsPerFile).map { case ((startFragPosition, endFragPosition), termsInFrag) =>
			val terms = termsInFrag.sortBy(_._1)
			val buf = new StringBuilder
			
			val lastAppended = terms.foldLeft(startFragPosition) { case (lastAppended, (termStart, termEnd, seqnum)) =>
				buf
					.append(source.substring(lastAppended, termStart))
					.append(preTag(seqnum))
					.append(source.substring(termStart,termEnd))
					.append(postTag(seqnum))
				
				termEnd
			}
			
			buf.append(source.substring(lastAppended, endFragPosition)).toString
		}
		
		fragments.toArray
	}
	
}