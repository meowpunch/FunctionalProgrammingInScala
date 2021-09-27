class Coder(words: List[String]):
  val mnemonics: Map[Char, String] = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  /* Maps a letter to the digit that it represents */
  private val charCode: Map[Char, Char] =
    for {
      (digit, chars) <- mnemonics
      char <- chars
    } yield char -> digit

  /* Maps a word to the digit string it can represent */
  private def wordCode(word: String): String =
    word.toUpperCase.map(charCode)

  /**
   * Maps a digit string to all words in the dictionary that represent it
   */
  private def wordsForNum: Map[String, List[String]] =
    words.groupBy(wordCode).withDefaultValue(Nil)

  /**
   * All ways to encode a number as a list of words
   */
  def encode(number: String): Set[List[String]] =
    if number.isEmpty then Set(Nil)
    else
      for {
        splitPoint <- (1 to number.length).toSet
        word <- wordsForNum(number.take(splitPoint))
        rest <- encode(number.drop(splitPoint))
      } yield word :: rest


def code(number: String) =
  val coder = Coder(List(
    "Scala", "Python", "Ruby", "C", "rocks", "socks", "sucks", "works", "pack"
  ))

  coder.encode(number).map(_.mkString(" "))

code("7225276257")

