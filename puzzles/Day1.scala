package puzzles

import scala.annotation.tailrec

object Day1 {

  def main(args: Array[String]): Unit = {
    println(solve)
  }

  private val input: Seq[String] = Utils.readInput("day1")
  private val numStrings: Map[String, Int] = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )
  private val revNumStrings = numStrings.map { case (k, v) => k.reverse -> v }

  def solve: Int = {
    input.map { line =>
      (findDigitInLine(line, numStrings) + findDigitInLine(line.reverse, revNumStrings)).toInt
    }.sum
  }

  @tailrec
  def findDigitInLine(line: String, valueMap: Map[String, Int]): String =
    if (line.head.isDigit) line.head.toString
    else {
      val numOp = valueMap.keys.find(line.startsWith)
      if (numOp.isDefined) valueMap(numOp.get).toString
      else findDigitInLine(line.tail, valueMap)
    }
}
