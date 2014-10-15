import scala.io.StdIn
import scala.util.Random

object McqApp extends App {
  val mcqType = StdIn.readLine("Select type: 1= Kanji reading, 2= Word reading").toInt
  val numQuestions = 3
  val numAnswers = 4
  val questions: Seq[Mcq] = mcqType match {
    case 1 => {
      val kyus: Map[String, Kyu] = ImportData.kanjisGroupedByKyu.map(_._1).map(k => k.label -> k)(scala.collection.breakOut)
      val kyuLabel = StdIn.readLine(s"Select kyu: [${kyus.map(_._1).mkString("|")}]")
      val kyu = kyus.getOrElse(kyuLabel, sys.error(s"Invalid kyu: $kyuLabel"))
      val kis = randomN(ImportData.kanjisGroupedByKyu(kyu), numQuestions)
      for (ki <- kis) yield {
        KanjiReadingMcq(ki, generateKanjiAnswers(numAnswers, ki))
      }
    }
    case 2 => {
      val kyus: Map[String, Kyu] = ImportData.wordsRatedGroupedByKyu.map(_._1).map(k => k.label -> k)(scala.collection.breakOut)
      val kyuLabel = StdIn.readLine(s"Select kyu: [${kyus.map(_._1).mkString("|")}]")
      val kyu = kyus.getOrElse(kyuLabel, sys.error(s"Invalid kyu: $kyuLabel"))
      val words = randomN(ImportData.wordsRatedGroupedByKyu(kyu), numQuestions)
      for (word <- words) yield {
        WordReadingMcq(word.word.word, generateWordAnswers(numAnswers, word.word.word))
      }
    }
    case x => sys.error(s"Invalid choice: $x")
  }

  for (question <- questions) {
    println(s"Reading for: ${question.question}")
    val answers = question.answers.toIndexedSeq
    println(answers.zipWithIndex.mkString(", "))
    val answIndex = StdIn.readInt()
    if (question.isCorrect(answers(answIndex))) {
      println("Hurray!")
    } else {
      println(s"Boo! was: ${question.solution}")
    }
  }

  def generateKanjiAnswers(numAnswers: Int, ki: KanjiInformation) = generateAnswers(numAnswers, ki.readings, ImportData.allKanjiReadings)

  def generateWordAnswers(numAnswers: Int, word: Word) = generateAnswers(numAnswers, Seq(word.reading), ImportData.allWordReadings)

  def generateAnswers[T](numAnswers: Int, rightAnswers: Seq[T], domain: Seq[T]) = {
    val allWrongAnswers = domain.diff(rightAnswers)
    randomN(allWrongAnswers, numAnswers)
  }

  def randomN[T](domain: Seq[T], numItems: Int) = {
    case class FoldingProgress(acc: Seq[T] = Nil, seen: Int = 0)
    domain.foldLeft(FoldingProgress()) {
      case (fp, t) => {
        val want = numItems - fp.acc.size
        val remaining = domain.size - fp.seen
        val add = want >= 0 && Random.nextInt(remaining) < want
        if (add) {
          FoldingProgress(fp.acc :+ t, fp.seen + 1)
        } else {
          FoldingProgress(fp.acc, fp.seen + 1)
        }
      }
    }.acc
  }
}