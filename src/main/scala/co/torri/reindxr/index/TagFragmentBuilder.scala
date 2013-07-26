package co.torri.reindxr.index

import org.apache.lucene.index.IndexReader
import org.apache.lucene.search.vectorhighlight.FieldFragList
import org.apache.lucene.search.vectorhighlight.SimpleFragmentsBuilder
import java.lang.{StringBuilder => JavaStringBuilder}

import scala.collection.JavaConversions._


case class TagFragmentBuilder(docs: Docs, snippetsOnly: Boolean, preTag: Int => String, postTag: Int => String) extends SimpleFragmentsBuilder(Array(preTag(0)), Array(postTag(0))) {
	
	private val maxFragmentsPerFile = 3
		
	override def createFragments(reader: IndexReader, docId: Int, fieldName: String, fieldFragList: FieldFragList, maxNumFragments: Int) : Array[String] = {
		
		val source = docs(reader.document(docId)).contents
		if (source.isEmpty) return Array[String]()

    val text = new MatchedText(source, fieldFragList)
		
    val fragments = text.terms.groupBy(_.line)
      .toArray
      .sortBy { case (line, terms) => - terms.size }
      .take(maxFragmentsPerFile)
      .map { case (line, terms) =>

        val snippetSize = line.size + terms.size * (preTag(0).size + postTag(0).size)
        val buf = new JavaStringBuilder(snippetSize)

        val lastAppended = terms
          .sortBy(_.start)
          .foldLeft(line.start) { case (lastAppended, term) =>
            buf
              .append(source, lastAppended, term.start)
              .append(preTag(term.number))
              .append(source, term.start, term.end)
              .append(postTag(term.number))

          term.end
        }

        buf.append(source, lastAppended, line.end).toString
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
		}
    .toArray
  }

  class MatchedText(val source: String, fieldFragList: FieldFragList) {

    val terms = for {
      fragInfo <- fieldFragList.getFragInfos
      subInfo <- fragInfo.getSubInfos
      termOffset <- subInfo.getTermsOffsets
    } yield MatchedTerm(this, termOffset.getStartOffset, termOffset.getEndOffset, subInfo.getSeqnum)

  }

  case class MatchedTerm(text: MatchedText, start: Int, end: Int, number: Int) {

    val line = {
      val startFragPosition =
        if (snippetsOnly) Some(text.source.lastIndexOf('\n', start)).map(index => if (index < 0) 0 else index + 1).get
        else 0
      val endFragPosition =
        if (snippetsOnly) Some(text.source.indexOf('\n', end)).map(index => if (index < 0) text.source.size else index).get
        else text.source.size
      Line(text, startFragPosition, endFragPosition)
    }
  }

  case class Line(text: MatchedText, start: Int, end: Int) {

    lazy val source = text.source.substring(start, end)

    def size = end - start
  }

}