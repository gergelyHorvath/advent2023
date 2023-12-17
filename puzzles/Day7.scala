package puzzles

object Day7 {
  def main(args: Array[String]): Unit = {
    val input: Seq[String] = Utils.readInput("day7")
//    val res = new Day7Solver(input, false).solve
    val res = new Day7Solver(input, true).solve
    println(res)
  }
}
class Day7Solver(val input: Seq[String], val hasJoker: Boolean) {
  private val cardTScore: Seq[Char] =  Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
  private val cardTScoreJoker: Seq[Char] =  Seq('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')
  private val cardScoreMap: Map[Char, Int] =
    (if (hasJoker) cardTScoreJoker else cardTScore)
      .reverse
      .zipWithIndex.toMap

  def solve: Int = {
    val t = input.map { i => i.split(" ").toList match { case cards :: score :: _ => cards -> score.toInt } }
      .sortBy { case (hand, _) => hand }(handOrdering)
    t foreach println
    input.map { i => i.split(" ").toList match { case cards :: score :: _ => cards -> score.toInt } }
      .sortBy { case (hand, _) => hand }(handOrdering)
      .zipWithIndex.map { case ((_, score), idx) => score * (idx + 1) }
      .sum
  }

  private def groupSameCards(hand: String): List[Int] = hand.toList.groupBy(identity).values.map(_.size).toList.sorted.reverse

  private def groupAndSort(hand: String): List[Int] = {
    if (hasJoker) {
      groupSameCards(hand.filter(_ != 'J').mkString) match {
        case _ :: rest => (5 - rest.sum) +: rest
        case Nil => List(5)
      }
    } else groupSameCards(hand)
  }

  private val primaryOrderFunc = (hand: String) =>
    groupAndSort(hand) match {
//    groupAndSort(hand.toList.groupBy(identity)) match {
      case 5 :: _ => 7
      case 4 :: _ => 6
      case 3 :: 2 :: _ => 5
      case 3 :: _ => 4
      case 2 :: 2 :: _ => 3
      case 2 :: _ => 2
      case _ => 1
    }

  private val secondaryOrderFunc = (hand: String) => hand.toList.map(c => cardScoreMap(c))
    .foldLeft(0) { case (acc, cu) => acc * 13 + cu }

  private val handOrdering: Ordering[String] = new Ordering[String] {
    override def compare(hand1: String, hand2: String): Int = {
      val primaryOrder = primaryOrderFunc(hand1) - primaryOrderFunc(hand2)
      if (primaryOrder == 0) secondaryOrderFunc(hand1) - secondaryOrderFunc(hand2) else primaryOrder
    }
  }

}


