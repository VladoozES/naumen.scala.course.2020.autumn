object Exercises {


  def reverse[T](seq: Seq[T]): Seq[T] = {
    seq.reverse
  }

  /**
   * https://ru.wikipedia.org/wiki/Числа_Фибоначчи
   *
   * @param idx
   * @return
   */
  def fibonacci4Index(idx: Int): Int = {
    var l = 0 :: 1 :: Nil
    for (i <- 2 to idx)
      l = l ::: List(l(i - 1) + l(i - 2))
    l(idx)
  }

  def fibonacci(idx: Int): Seq[Int] = {
    var l : Seq[Int] = Seq[Int](0, 1)
    for (i <- 2 to idx)
      l = l ++ Seq(l(i - 1) + l(i - 2))
    l
  }

  lazy val MORSE = Map("A" -> ".-", "B" -> "-...", "C" -> "-.-.", "D" -> "-..", "E" -> ".", "F" -> "..-.",
    "G" -> "--.", "H" -> "....", "I" -> "..", "J" -> ".---", "K" -> "-.-", "L" -> ".-..",
    "M" -> "--", "N" -> "-.", "O" -> "---", "P" -> ".--.", "Q" -> "--.-", "R" -> ".-.",
    "S" -> "...", "T" -> "-", "U" -> "..-", "V" -> "...-", "W" -> ".--", "X" -> "-..-",
    "Y" -> "-.--", "Z" -> "--..")

  def morse(text: String): String = {
    var res_str : String = ""
    var without_space : Boolean = true
    for (a <- text) {
      if (!MORSE.contains(a.toUpper.toString)) {
        if (a == ' ') res_str += "   "
        else res_str += a
        without_space = true
      }
      else {
        if (without_space) res_str += MORSE(a.toUpper.toString)
        else res_str += " " + MORSE(a.toUpper.toString)
        without_space = false
      }
    }
    res_str
  }


  def wordReverse(text: String): String = {
    var res = text
    val temp = text.split(Array('!', '.', ' ', ',')).filter(element => !element.isEmpty)
    for (i <- temp) {
      while (res.contains(i)){
        val start_index = res.indexOf(i)
        val input_text = if (i(0).isUpper) {
          i.reverse (0).toUpper + i.reverse.toLowerCase.substring(1)} else i.reverse
        res = res.substring(0, start_index) + input_text + res.substring(start_index + i.length)
      }
    }
    res
  }

}
