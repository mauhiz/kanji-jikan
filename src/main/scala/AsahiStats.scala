
object AsahiStats extends CrawlerStats {

  protected def extractor = "div.ArticleText p"

  protected def allUrls = Seq(
    "http://www.asahi.com/articles/ASG8C6KRKG8CUCVL01Y.html",
    "http://www.asahi.com/articles/ASG8D6SW3G8DTQIP008.html",
    "http://www.asahi.com/articles/ASG8D63DRG8DULOB01Q.html",
    "http://www.asahi.com/articles/ASG6W2GB1G6WUCVL003.html"
  )
}








