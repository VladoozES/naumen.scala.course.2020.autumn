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

  def morse(text: String): String = ???


  def wordReverse(text: String): String = ???

}
