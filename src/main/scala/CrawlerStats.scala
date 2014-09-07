import java.nio.file.Paths

import upickle._

import scala.util.Try

trait CrawlerStats extends HtmlExtraction with Analyzing with ReadFile with WriteFile {

  protected def allUrls: Seq[String]

  private def extractArticleInformation(url: String): ArticleInformation = {
    val doc = fetch(url)
    val words = for {
      paragraph <- extractText(doc)
      word <- tokenize(paragraph)
    } yield word
    val dss = dictionarySections(words)
    ArticleInformation(url, dss)
  }

  protected val dataPath = Paths.get("data", "articles.json")

  def runForUrls(urls: Seq[String]) = {
    for {
      url <- urls
    } yield {
      extractArticleInformation(url)
    }
  }

  def retrieve(force: Boolean = false): Seq[ArticleInformation] = {
    val existingData = if (force) None
    else readIfPossible(dataPath)(data => Try {
      read[Seq[ArticleInformation]](data)
    }.toOption)
    existingData.fold {
      val articleInfos = runForUrls(allUrls)
      save(articleInfos)
      articleInfos
    } {
      existingArticleInfos =>
        val alreadyRetrieved = existingArticleInfos.map(_.url).toSet
        val missingUrls = allUrls.filterNot(alreadyRetrieved.contains)
        if (missingUrls.nonEmpty) {
          val articleInfos = runForUrls(missingUrls) ++ existingArticleInfos
          save(articleInfos)
          articleInfos
        } else {
          existingArticleInfos
        }
    }
  }

  private def save(ais: Seq[ArticleInformation]) = {
    val data = write(ais)
    writeIfPossible(dataPath, data)
  }
}
