package puzzles

object Day2 {
  private final case class Game(id: Int, red: Seq[Int], green: Seq[Int], blue: Seq[Int])
  private val input: Seq[String] = Utils.readInput("day2")

  def main(args: Array[String]): Unit = {
    println(solve1)
    println(solve2)
  }

  private def parseGame(game: String): Game = {
    val getColor = (color: String) => s"\\d* $color".r.findAllIn(game).map(_.split(" ").head.toInt).toSeq
    val id = "\\d*:".r.findFirstIn(game).get.init.toInt
    Game(id, getColor("red"), getColor("green"), getColor("blue"))
  }

  def solve1: Int =
    input.map(parseGame).filter{ g =>
      g.red.forall(_ <= 12) && g.green.forall(_ <= 13) && g.blue.forall(_ <= 14)
    }.map(_.id).sum

  def solve2: Int = input.map(parseGame).map(g => g.red.max * g.blue.max * g.green.max).sum
}
