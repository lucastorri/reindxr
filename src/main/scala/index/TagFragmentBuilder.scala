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


case class TagFragmentBuilder(val preTag: String, val postTag: String) extends SimpleFragmentsBuilder(Array(preTag), Array(postTag)) {
	
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
			} yield (termOffset.getStartOffset, termOffset.getEndOffset)
		
		val sourceChars = source.toCharArray
			
		val fragments = termPositions.groupBy { case (termStart, termEnd) =>
			val startFragPosition = Some(sourceChars.lastIndexOf('\n', termStart)).map(index => if (index < 0) 0 else index + 1).get
			val endFragPosition = Some(sourceChars.indexOf('\n', termEnd)).map(index => if (index < 0) source.size else index).get
			(startFragPosition, endFragPosition)
		}.toList.reverseMap { case ((startFragPosition, endFragPosition), termsInFrag) =>
			val terms = termsInFrag.sortBy(_._1)
			val buf = new StringBuilder
			
			val lastAppended = terms.foldLeft(startFragPosition) { case (lastAppended, (termStart, termEnd)) =>
				buf
					.append(source.substring(lastAppended, termStart))
					.append(preTag)
					.append(source.substring(termStart,termEnd))
					.append(postTag)
				
				termEnd
			}
			
			buf.append(source.substring(lastAppended, endFragPosition)).toString
		}
		
		//maxNumFragments
		return fragments.toArray
	}
	
    /*
	@Override
    public String createFragment(IndexReader reader, int docId, String fieldName, FieldFragList fieldFragList) throws IOException {
		System.out.println("hello");
        // read the source string back from the index
        
        // find the occurrences of the matched phrase
        List<Range> termPositions = new ArrayList<Range>();
        for (WeightedFragInfo fragInfo : fieldFragList.getFragInfos()) {
            List<SubInfo> subInfos = fragInfo.getSubInfos();
            for (SubInfo subInfo : subInfos) {
                List<Toffs> termOffsets = subInfo.getTermsOffsets();
                for (Toffs termOffset : termOffsets) {
                    Range termPosition = new IntRange(
                        termOffset.getStartOffset(), termOffset.getEndOffset());
                    termPositions.add(termPosition);
                }
            }
        }
        if (termPositions.size() == 0) {
            return StringUtils.substring(source, 0, fragLen);
        }
        int startFragPosition = 0;
        int endFragPosition = 0;
        // read back on the char array until we find a period,
        // then read front until we find a letter/digit. This
        // is our fragment start position. If no period found,
        // then this must be the first sentence, start here.
        if (fragLen < 0) {
            // we don't need a fragLen for titles, take them whole
            // so in this case fragLen should be -1.
            endFragPosition = source.length();
        } else {
            int startTermPosition = termPositions.get(0).getMinimumInteger();
            char[] sourceChars = source.toCharArray();
            for (int i = startTermPosition; i >= 0; i--) {
                if (sourceChars[i] == '\n') {
                    startFragPosition = i;
                    break;
                }
            }
            for (int i = startFragPosition + 1; i < sourceChars.length; i++) {
                if (sourceChars[i] == '\n') {
                    endFragPosition = i;
                    break;
                }
            }
        }
		System.out.println("source: " + source);
		System.out.println("b: " + startFragPosition);
		System.out.println("e: " + endFragPosition);	
        // return the substring bounded by start and end, highlighting
        // the matched phrase
        final Range fragRange = new IntRange(startFragPosition, endFragPosition);
        CollectionUtils.filter(termPositions, new Predicate() {
            public boolean evaluate(Object obj) {
                Range r = (Range) obj;
                return (fragRange.containsRange(r));
            }
        });
        if (termPositions.size() == 0) {
            // unlikely, since we are pretty sure that there is at least
            // one term position in our fragRange, but just being paranoid
            return StringUtils.substring(source, startFragPosition, endFragPosition);
        }
        StringBuilder buf = new StringBuilder();
        buf.append(StringUtils.substring(source, startFragPosition, endFragPosition));
        int numHighlights = termPositions.size();
	    for (int i = 0; i < numHighlights; i++) {
            buf.append(pretag).append(StringUtils.substring(source,
                                    termPositions.get(i).getMinimumInteger(),
                                    termPositions.get(i).getMaximumInteger())).append(posttag);
            if (i < numHighlights - 1) {
                buf.append(StringUtils.substring(source, termPositions.get(i).getMaximumInteger(), 
                                        termPositions.get(i+1).getMinimumInteger()));
            }
        }
        buf.append(StringUtils.substring(source, termPositions.get(numHighlights-1).getMaximumInteger(), 
                                fragRange.getMaximumInteger()));
        return buf.toString();
    }    
	*/
}