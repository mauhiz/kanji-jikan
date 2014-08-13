import java.io.StringReader
import java.net.URL

import org.apache.lucene.analysis._
import org.apache.lucene.analysis.ja.JapaneseAnalyzer
import org.apache.lucene.analysis.ja.tokenattributes._
import org.apache.lucene.analysis.tokenattributes._
import org.apache.lucene.util.Version
import org.jsoup._
import org.jsoup.nodes.Document

import scala.collection.convert.Wrappers._
import scala.collection.mutable.ArrayBuffer

object AsahiStats extends App with HtmlExtraction with Analyzing {

  def extractor = "div.ArticleText p"
  val urls = Set(
    "http://www.asahi.com/articles/ASG8C6KRKG8CUCVL01Y.html",
    "http://www.asahi.com/articles/ASG8D6SW3G8DTQIP008.html",
    "http://www.asahi.com/articles/ASG8D63DRG8DULOB01Q.html",
    "http://www.asahi.com/articles/ASG6W2GB1G6WUCVL003.html"
  ).toSeq

  val words = for {
    url <- urls
    doc = fetch(url)
    paragraph <- extractText(doc)
    word <- tokenize(paragraph)
  } yield {
    word
  }
  val dicts = dictionarySections(words)
  for (dict <- dicts) {
    println(dict)
  }

}

case class WordCount(count: Int, word: Word)

case class Word(writing: String, reading: String, partOfSpeech: String)

trait HtmlExtraction {

  def fetch(url: String, timeoutMs: Int = 10000): Document = Jsoup.parse(new URL(url), timeoutMs)

  def extractor: String

  def extractText(doc: Document) = {
    val paragraphs = doc.select(extractor)
    JListWrapper(paragraphs).toSeq.map(_.text())
  }
}

trait Analyzing {

  def shouldExcludePartOfSpeech(pos: String): Boolean = {
    pos.contains("固有名詞")
  }


  def dictionarySections(words: Seq[Word]) =  words.groupBy(_.partOfSpeech).mapValues(countWords).toSeq.sortBy(_._1)

  def printAttributes(stream: TokenStream): Unit = {
    val it = stream.getAttributeImplsIterator
    while (it.hasNext) {
      val n = it.next()
      println(n.getClass + ": " + n)
    }
  }

  def countWords(words: Seq[Word]): Seq[WordCount] = {
    words.groupBy(identity[Word]).map {
      case (word, sameWords) => WordCount(sameWords.size, word)
    }.toSeq.sortBy(-_.count)
  }

  lazy val analyzer = new JapaneseAnalyzer(Version.LUCENE_36)

  def tokenize(string: String) = {
    val tokens = new ArrayBuffer[Word]
    val stream = analyzer.tokenStream(null, new StringReader(string))
    stream.reset()
    while (stream.incrementToken()) {
      //printAttributes(stream)
      val partOfSpeech = stream.getAttribute(classOf[PartOfSpeechAttribute]).getPartOfSpeech
      if (!shouldExcludePartOfSpeech(partOfSpeech)) {
        val token = stream.getAttribute(classOf[CharTermAttribute]).toString
        val reading = stream.getAttribute(classOf[ReadingAttribute]).getReading
        tokens += Word(token, reading, partOfSpeech)
      }
    }
    tokens.toSeq
  }
}
