
object KanjiDataRetrieval extends App with HtmlExtraction with Analyzing {
  val kyus = Seq("01j", "02j") ++ (1 to 10).map(i => f"$i%02d").sorted.reverse

  def urlForKyu(kyu: String) = s"http://jiten.go-kanken.com/cat/kyu$kyu.html"

  def extractor = "table.kyumenu tr td a"

  val kanjis = for {
    kyu <- kyus
    url = urlForKyu(kyu)
    doc = fetch(url)
    item <- extractText(doc)
  } yield {
    item.trim -> kyu
  }

  println(kanjis.toMap)
  for ((kanji, kyu) <- kanjis) {
    val tokenInfo = tokenize(kanji)
    if (tokenInfo.nonEmpty) {
      println(kyu + "\t" + tokenInfo.head)
    }
  }
}
