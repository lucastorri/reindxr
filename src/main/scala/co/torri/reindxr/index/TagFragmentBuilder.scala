package co.torri.reindxr.index

import java.lang.{StringBuilder => JavaStringBuilder}

import org.apache.lucene.index.IndexReader
import org.apache.lucene.search.vectorhighlight.{FieldFragList, SimpleFragmentsBuilder}

import scala.jdk.CollectionConverters._


case class TagFragmentBuilder(docs: Docs, snippetsOnly: Boolean, preTag: Int => String, postTag: Int => String) extends SimpleFragmentsBuilder(Array(preTag(0)), Array(postTag(0))) {

  private val maxFragmentsPerFile = 3

  val tagsSize: Int = preTag(0).length + postTag(0).length

  override def createFragments(reader: IndexReader, docId: Int, fieldName: String, fieldFragList: FieldFragList, maxNumFragments: Int): Array[String] = {

    val source = docs(reader.document(docId)).contents
    if (source.isEmpty) return Array[String]()

    val text = new MatchedText(source, fieldFragList)

    val fragments = text.terms.groupBy(_.line)
      .toArray
      .sortBy { case (_, terms) => -terms.size }
      .take(maxFragmentsPerFile)
      .map { case (line, terms) =>

        val snippetSize = line.size + (terms.size * tagsSize)
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

    if (snippetsOnly && fragments.length < maxFragmentsPerFile) {
      val b = fragments.toBuffer
      source.linesWithSeparators.toList.takeWhile { l =>
        if (!l.trim.isEmpty && !b.contains(l)) {
          b += l
        }
        b.size < maxFragmentsPerFile
      }
      b.toArray
    } else if (!snippetsOnly && fragments.isEmpty) {
      Array(source)
    } else {
      fragments
    }
  }

  class MatchedText(val source: String, fieldFragList: FieldFragList) {

    val terms: Seq[MatchedTerm] = {
      val innerTerms = for {
        fragInfo <- fieldFragList.getFragInfos.asScala
        subInfo <- fragInfo.getSubInfos.asScala
        termOffset <- subInfo.getTermsOffsets.asScala
      } yield MatchedTerm(this, termOffset.getStartOffset, termOffset.getEndOffset, subInfo.getSeqnum)

      innerTerms.toSeq
    }

  }

  case class MatchedTerm(text: MatchedText, start: Int, end: Int, number: Int) {

    val stop = '\n'

    val line: Line = {
      val startFragPosition =
        if (snippetsOnly) Some(text.source.lastIndexOf(stop, start)).filter(_ >= 0).map(_ + 1).getOrElse(0)
        else 0
      val endFragPosition =
        if (snippetsOnly) Some(text.source.indexOf(stop, end)).filter(_ >= 0).getOrElse(text.source.length)
        else text.source.length
      Line(text, startFragPosition, endFragPosition)
    }
  }

  case class Line(text: MatchedText, start: Int, end: Int) {

    lazy val source: String = text.source.substring(start, end)

    def size: Int = end - start
  }

}