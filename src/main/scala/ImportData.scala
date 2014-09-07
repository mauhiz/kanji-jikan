
object ImportData extends App {
  def wordsRatedGroupedByKyu = {
    val kanjis: Map[Char, KanjiInformation] = KanjiDataRetrieval.retrieve().map(ki => ki.kanji -> ki)(scala.collection.breakOut)
    val articleInfos = Seq[CrawlerStats](AsahiStats, MainichiStats).flatMap(_.retrieve())
    val dicts = combineArticleInfo(articleInfos)
    dicts.flatMap(_.wordCounts).map(rate(_, kanjis)).flatten.filter(_.word.kyu.order < noobKyu.order).groupBy(_.word.kyu).toSeq.sortBy(-_._1.order)
  }

  for (group <- wordsRatedGroupedByKyu) {
    println()
    println(group._1.label)
    println(group._2.sortBy(-_.count))
  }

  def isKana(c: Char) = c >= '\u3000' && c <= '\u30ff'

  def isAscii(c: Char) = c <= 0xff

  val noobKyu = Kyu("Non-kanji", 1000)

  def rate(wc: WordCount, kanjis: Map[Char, KanjiInformation]): Option[RatedWordWithCount] = {
    val wordKyus = wc.word.writing.toCharArray.map {
      case c if isKana(c) || isAscii(c) => noobKyu
      case c => kanjis.get(c).map(_.kyu).getOrElse {
        println(s"Unknown kanji: $c (${c.toHexString})")
        Kyu("Unknown kanji", 0)
      }
    }
    if (wordKyus.isEmpty) None else Some(RatedWordWithCount(wc.count, RatedWord(wc.word, wordKyus.minBy(_.order))))
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
