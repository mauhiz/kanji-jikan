import java.nio.file._

import upickle._

import scala.util.Try

object KanjiDataRetrieval extends HtmlExtraction with Analyzing with ReadFile with WriteFile {
  // TODO get the order from the index page
  lazy val kyus: Seq[Kyu] = Seq(Kyu("01j", 1.5), Kyu("02j", 2.5)) ++ (for {
    i <- 1 to 10
  } yield {
    Kyu(f"$i%02d", i)
  })

  private def urlForKyu(kyu: Kyu) = s"http://jiten.go-kanken.com/cat/kyu${kyu.label}.html"

  protected def extractor = "table.kyumenu tr td a"

  private val dataPath = Paths.get("data", "kanjis.json")

  def retrieve(force: Boolean = false): Seq[KanjiInformation] = {
    val existingData = if (force) None
    else readIfPossible(dataPath)(data => Try {
      read[Seq[KanjiInformation]](data)
    }.toOption.filter(_.nonEmpty))
    existingData.getOrElse {
      val kanjiOpts = for {
        kyu <- kyus
        url = urlForKyu(kyu)
        doc = fetch(url)
        item <- extractText(doc)
      } yield {
        val kanjiStr = item.trim
        val chars = kanjiStr.toCharArray
        chars match {
          case Array(c) =>
            tokenize(kanjiStr) match {
              case Seq(tokenInfo) => Some(KanjiInformation(c, Some(tokenInfo.partOfSpeech), Seq(tokenInfo.word.reading), kyu))
              case other => Some(KanjiInformation(c, None, Seq.empty, kyu))
            }
          case _ =>
            println(s"Skipping invalid kanji $kanjiStr of length ${chars.length}")
            None
        }
      }
      val kanjis = kanjiOpts.flatten
      save(kanjis)
      kanjis
    }
  }

  private def save(kanjis: Seq[KanjiInformation]) = {
    val data = write(kanjis)
    writeIfPossible(dataPath, data)
  }

}