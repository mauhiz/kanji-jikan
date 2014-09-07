import java.net.URL

import org.jsoup._
import org.jsoup.nodes.Document

import scala.collection.convert.Wrappers._

trait HtmlExtraction {

  protected def fetch(url: String, timeoutMs: Int = 10000): Document = {
    println(s"fetching URL: $url")
    Jsoup.parse(new URL(url), timeoutMs)
  }

  protected def extractor: String

  protected def extractText(doc: Document): Seq[String] = {
    val paragraphs = doc.select(extractor)
    JListWrapper(paragraphs).toSeq.map(_.text())
  }
}
