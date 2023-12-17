package puzzles

import scala.annotation.tailrec

object Day4 {
  def main(args: Array[String]): Unit = {
//    println(solve1)
    println(solve2)
  }

  private val input: Seq[String] = Utils.readInput("day4")
  def solve1: Int = input.map(c => getScore(parseCard(c))).sum
  def solve2: Int = countCopies(input.map(c => Card(getMatches(parseCard(c)))))

  final case class Card(score: Int, copies: Int = 1)

  @tailrec
  def countCopies(cards: Seq[Card], counter: Int = 0): Int = {
    if (cards.isEmpty) counter
    else {
      val current = cards.head
      val newCounter = counter + current.copies
      val updatedCards = cards.tail.take(current.score).map(c => Card(c.score, c.copies + current.copies))
      countCopies(updatedCards ++ cards.drop(current.score + 1), newCounter)
    }
  }

  private def getScore(card:(Seq[Int], Seq[Int])): Int = {
    val matches = getMatches(card)
    if (matches > 0) Math.pow(2, matches - 1).toInt else 0
  }

  private def getMatches(card: (Seq[Int], Seq[Int])): Int =
    card._2.count(card._1.contains)

  def parseCard(card: String): (Seq[Int], Seq[Int]) = {
    val splitString = card.split(": ").last.split(" \\| ")
      .map(_.trim.replace("  ", " ").split(" ").toSeq.map(_.toInt))
    (splitString.head, splitString.last)
  }
}
