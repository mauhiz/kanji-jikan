

object MainichiStats extends CrawlerStats {

  protected def extractor = "div.NewsBody p"

  protected def allUrls = Seq(
    "http://mainichi.jp/select/news/20140813k0000m040111000c.html",
    "http://mainichi.jp/select/news/20140813k0000m040157000c.html"
  )

}








