
object ImportData {

  val NoobKyu = Kyu("Non-kanji", 1000d)

  lazy val kanjis: Map[Char, KanjiInformation] = KanjiDataRetrieval.retrieve().map(ki => ki.kanji -> ki)(scala.collection.breakOut)
  lazy val kanjisGroupedByKyu: Map[Kyu, Seq[KanjiInformation]] = kanjis.map(_._2)(scala.collection.breakOut).filter(_.readings.nonEmpty).groupBy(_.kyu)
  lazy val allKanjiReadings: Seq[String] = kanjis.flatMap(_._2.readings)(scala.collection.breakOut)

  lazy val dicts: Seq[DictionarySection] = {
    val articleInfos = Seq[CrawlerStats](AsahiStats, MainichiStats).flatMap(_.retrieve())
    combineArticleInfo(articleInfos)
  }
  lazy val wordsRatedGroupedByKyu: Map[Kyu, Seq[RatedWordWithCount]] = dicts.flatMap(_.wordCounts).map(rate).flatten.filter(_.word.kyu.order < NoobKyu.order).groupBy(_.word.kyu)
  lazy val allWordReadings: Seq[String] = dicts.flatMap(_.wordCounts).map(_.word.reading)(scala.collection.breakOut)


  def isKana(c: Char) = c >= '\u3000' && c <= '\u30ff'

  def isAscii(c: Char) = c <= 0xff

  def rateChar(char: Char): Kyu = char match {
    case c if isKana(c) || isAscii(c) => NoobKyu
    case c => kanjis.get(c) match {
      case None => {
        println(s"Unknown kanji: $c (${c.toHexString})")
        Kyu("Unknown kanji", 0)
      }
      case Some(ki) => ki.kyu
    }
  }

  def rateWord(writing: String): Option[Kyu] = {
    val wordKyus = writing.map(rateChar)

    if (wordKyus.isEmpty) None
    else {
      val hardestKyu = wordKyus.minBy { kyu =>
        if (kyu == null) {
          sys.error(s"null kyu for word: $writing, kyus: $wordKyus")
        }
        else kyu.order
      }
      Some(hardestKyu)
    }
  }

  def rate(wc: WordCount): Option[RatedWordWithCount] = {
    rateWord(wc.word.writing).map { hardestKyu =>
      RatedWordWithCount(wc.count, RatedWord(wc.word, hardestKyu))
    }
  }

  def mergeWordCounts(itemsToMerge: Seq[WordCount]): Seq[WordCount] = {
    itemsToMerge.groupBy(_.word).map {
      case (word, wcs) =>
        WordCount(wcs.map(_.count).sum, word)
    }(scala.collection.breakOut)
  }

  def combineArticleInfo(articleInfos: Seq[ArticleInformation]): Seq[DictionarySection] = {
    val dss = articleInfos.flatMap(_.dictionarySections)
    val dssByPos = dss.groupBy(_.partOfSpeech)
    dssByPos.map {
      case (pos, posDss) =>
        val mergedWcs = posDss.foldLeft(Seq.empty[WordCount]) {
          case (agg, ds) => mergeWordCounts(agg ++ ds.wordCounts)
        }
        DictionarySection(pos, mergedWcs.sortBy(-_.count))
    }(scala.collection.breakOut).sortBy(_.partOfSpeech)
  }
}

object ImportDataApp extends App {

  def printStats() = {
    for (group <- ImportData.wordsRatedGroupedByKyu.toSeq.sortBy(-_._1.order)) {
      println()
      println(group._1.label)
      println(group._2.sortBy(-_.count))
    }
  }
  printStats()
}