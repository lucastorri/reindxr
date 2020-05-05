package co.torri.reindxr.take2

import java.lang.{StringBuilder => JavaStringBuilder}

import cats.implicits._
import org.apache.lucene.index.IndexReader
import org.apache.lucene.search.vectorhighlight.{FieldFragList, SimpleFragmentsBuilder}

import scala.jdk.CollectionConverters._


case class TagFragmentBuilder2(content: String, snippetsOnly: Boolean, preTag: Int => String, postTag: Int => String) extends SimpleFragmentsBuilder(Array(preTag(0)), Array(postTag(0))) {

  private val tagsUsualSize: Int = preTag(0).length + postTag(0).length

  override def createFragments(reader: IndexReader, docId: Int, fieldName: String, fieldFragList: FieldFragList, maxNumFragments: Int): Array[String] = {
    val terms = extractTerms(fieldFragList)
    if (terms.isEmpty) {
      Array.empty
    } else {
      val fragments = terms.groupBy(_.line)
        .toSeq
        .sortBy { case (_, terms) => -terms.size }
        .take(maxNumFragments)
        .map { case (line, terms) =>

          val snippetSize = line.size + (terms.size * tagsUsualSize)
          val buf = new JavaStringBuilder(snippetSize)

          val lastAppended = terms
            .sortBy(_.start)
            .foldLeft(line.start) { case (lastAppended, term) =>
              buf
                .append(content, lastAppended, term.start)
                .append(preTag(term.number))
                .append(content, term.start, term.end)
                .append(postTag(term.number))

              term.end
            }

          buf.append(content, lastAppended, line.end).toString
        }
        .toArray

      if (snippetsOnly && fragments.length < maxNumFragments) {
        val b = fragments.toBuffer
        content.linesWithSeparators.toList.takeWhile { l =>
          if (!l.trim.isEmpty && !b.contains(l)) {
            b += l
          }
          b.size < maxNumFragments
        }
        b.toArray
      } else if (!snippetsOnly && fragments.isEmpty) {
        Array(content)
      } else {
        fragments
      }
    }
  }

  private def extractTerms(fieldFragList: FieldFragList): Seq[MatchedTerm] = {
    if (content.isEmpty) {
      return Seq.empty
    }

    val innerTerms = for {
      fragInfo <- fieldFragList.getFragInfos.asScala
      subInfo <- fragInfo.getSubInfos.asScala
      termOffset <- subInfo.getTermsOffsets.asScala
    } yield MatchedTerm(content, termOffset.getStartOffset, termOffset.getEndOffset, subInfo.getSeqnum)

    innerTerms.toSeq
  }

  case class MatchedTerm(source: String, start: Int, end: Int, number: Int) {

    val stop = '\n'

    val line: Line = {
      val startFragPosition =
        if (snippetsOnly) Some(source.lastIndexOf(stop, start)).filter(_ >= 0).map(_ + 1).getOrElse(0)
        else 0
      val endFragPosition =
        if (snippetsOnly) Some(source.indexOf(stop, end)).filter(_ >= 0).getOrElse(source.length)
        else source.length
      Line(source, startFragPosition, endFragPosition)
    }
  }

  case class Line(source: String, start: Int, end: Int) {

    lazy val content: String = source.substring(start, end)

    def size: Int = end - start
  }

}
