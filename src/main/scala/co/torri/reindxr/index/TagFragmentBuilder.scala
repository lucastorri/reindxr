package co.torri.reindxr.index

import org.apache.lucene.index.IndexReader
import org.apache.lucene.search.vectorhighlight.FieldFragList
import org.apache.lucene.search.vectorhighlight.SimpleFragmentsBuilder

import scala.collection.JavaConversions._


case class TagFragmentBuilder(docFactory: DocFactory, snippetsOnly: Boolean, preTag: Int => String, postTag: Int => String) extends SimpleFragmentsBuilder(Array(preTag(0)), Array(postTag(0))) {
	
	private val maxFragmentsPerFile = 3
		
	override def createFragments(reader: IndexReader, docId: Int, fieldName: String, fieldFragList: FieldFragList, maxNumFragments: Int) : Array[String] = {
		
		val source = docFactory(reader.document(docId)).contents
		if (source.isEmpty) {
			return Array[String]()
		}
		
        val termPositions = 
			for {
				fragInfo <- fieldFragList.getFragInfos
				subInfo <- fragInfo.getSubInfos
				termOffset <- subInfo.getTermsOffsets
			} yield (termOffset.getStartOffset, termOffset.getEndOffset, subInfo.getSeqnum)
					
		var fragments = termPositions.groupBy { case (termStart, termEnd, seqnum) =>
			val startFragPosition = 
				if (snippetsOnly) Some(source.lastIndexOf('\n', termStart)).map(index => if (index < 0) 0 else index + 1).get
				else 0
			val endFragPosition = 
				if (snippetsOnly) Some(source.indexOf('\n', termEnd)).map(index => if (index < 0) source.size else index).get
				else source.size
			(startFragPosition, endFragPosition)
		}.toList.sortBy(- _._2.size).take(maxFragmentsPerFile).map { case ((startFragPosition, endFragPosition), termsInFrag) =>
			val terms = termsInFrag.sortBy(_._1)
			val buf = new StringBuilder
			
			val lastAppended = terms.foldLeft(startFragPosition) { case (lastAppended, (termStart, termEnd, seqnum)) =>
				buf
					.append(source.substring(lastAppended, termStart))
					.append(preTag(seqnum))
					.append(source.substring(termStart, termEnd))
					.append(postTag(seqnum))
				
				termEnd
			}
			
			buf.append(source.substring(lastAppended, endFragPosition)).toString
		}
		
		if (snippetsOnly && fragments.size < maxFragmentsPerFile) {
			val b = fragments.toBuffer
			source.linesWithSeparators.toList.takeWhile { l =>
			  	if (!l.trim.isEmpty && !b.contains(l)) {
			  		b += l
			  	}
			  	(b.size < maxFragmentsPerFile)
			}
			b.toArray
		} else if (!snippetsOnly && fragments.isEmpty) {
			Array(source)
		} else {
			fragments
		}.toArray
    }
	
}