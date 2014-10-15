case class Word(writing: String, reading: String)

case class GrammarElement(word: Word, partOfSpeech: String)

case class WordCount(count: Int, word: Word)

case class DictionarySection(partOfSpeech: String, wordCounts: Seq[WordCount])

case class ArticleInformation(url: String, dictionarySections: Seq[DictionarySection])

case class Kyu(label: String, order: Double)

case class KanjiInformation(kanji: Char, partOfSpeech: Option[String], readings: Seq[String], kyu: Kyu)

case class RatedWord(word: Word, kyu: Kyu)

case class RatedWordWithCount(count: Int, word: RatedWord)

case class User(name: String)

case class Knowledge(seen: Int, sucess: Int, error: Int)

case class KanjiStatus(kanji: Char, knowledge: Knowledge)

case class WordStatus(word: Word, knowledge: Knowledge)

case class UserProgress(user: User, kanjis: Seq[KanjiStatus], words: Seq[WordStatus])

sealed trait Mcq {
  def answers: Seq[String]

  def question: String

  def isCorrect(answer: String): Boolean

  def solution: String
}

case class KanjiReadingMcq(kanjiInfo: KanjiInformation, wrongAnswers: Seq[String]) extends Mcq {
  require(kanjiInfo.readings.nonEmpty)

  lazy val answers = util.Random.shuffle(wrongAnswers :+ kanjiInfo.readings.head)

  def question = kanjiInfo.kanji.toString

  def isCorrect(answer: String) =  kanjiInfo.readings.contains(answer)

  def solution = kanjiInfo.toString
}

case class WordReadingMcq(word: Word, wrongAnswers: Seq[String]) extends Mcq {
  lazy val answers = util.Random.shuffle(wrongAnswers :+ word.reading)

  def question = word.writing

  def isCorrect(answer: String) = answer == word.reading

  def solution = word.toString
}

case class Quiz(questions: Seq[Mcq])


