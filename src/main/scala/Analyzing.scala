import java.io.StringReader

import org.apache.lucene.analysis._
import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.analysis.ja.tokenattributes._
import org.apache.lucene.analysis.tokenattributes._
import org.apache.lucene.util.Version

import scala.collection.mutable.ArrayBuffer

trait Analyzing {

  protected def shouldExcludePartOfSpeech(pos: String): Boolean = {
    pos.contains("固有名詞")
  }

  protected def dictionarySections(words: Seq[GrammarElement]): Seq[DictionarySection] = {
    words.groupBy(_.partOfSpeech).map {
      case (pos, ge) =>
        val wcs = countWords(ge.map(_.word))
        DictionarySection(pos, wcs)
    }(scala.collection.breakOut).sortBy(_.partOfSpeech)
  }

  protected def printAttributes(stream: TokenStream): Unit = {
    val it = stream.getAttributeImplsIterator
    while (it.hasNext) {
      val n = it.next()
      println(n.getClass + ": " + n)
    }
  }

  protected def countWords(words: Seq[Word]): Seq[WordCount] = {
    words.groupBy(identity[Word]).map {
      case (word, sameWords) => WordCount(sameWords.size, word)
    }(scala.collection.breakOut).sortBy(-_.count)
  }

  protected lazy val analyzer = new JapaneseAnalyzer(Version.LUCENE_36)

  protected def tokenize(string: String): Seq[GrammarElement] = {
    val stream = analyzer.tokenStream(null, new StringReader(string))
    try {
      stream.reset()
      val tokens = new ArrayBuffer[GrammarElement]
      while (stream.incrementToken()) {
        val partOfSpeech = stream.getAttribute(classOf[PartOfSpeechAttribute]).getPartOfSpeech
        if (!shouldExcludePartOfSpeech(partOfSpeech)) {
          val token = stream.getAttribute(classOf[CharTermAttribute]).toString
          val reading = stream.getAttribute(classOf[ReadingAttribute]).getReading
          tokens += GrammarElement(Word(token, Option(reading).getOrElse(token)), partOfSpeech)
        }
      }
      tokens
    } finally {
      stream.close()
    }
  }
}
