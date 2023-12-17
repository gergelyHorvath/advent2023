package puzzles

object Day6 {
  private val input: Seq[String] = Utils.readInput("day6")

  def main(args: Array[String]): Unit = {
    println(solveQ1)
    //Q2
    println(findWinningOptions(202107611381458d, 44826981d))
  }

  def solveQ1: Double = parseInputs.map(r => findWinningOptions(r.distance, r.time)).product

  final case class Race(distance: Int, time: Int)

  private def findWinningOptions(s: Double, t: Double): Double = {
    val D = Math.sqrt(Math.pow(t, 2) - 4 * s)
    val limits = Seq((t + D) / 2, (t - D) / 2)
    val lower = if (limits.min.isValidInt) limits.min + 1 else limits.min.ceil
    val upper = if (limits.max.isValidInt) limits.max - 1 else limits.max.floor
    upper - lower + 1
  }

  private def parseInputs: Seq[Race] = {
    val findNumbersIn = (text: String) => "\\d+".r.findAllIn(text).map(_.toInt)
    val distances =  findNumbersIn(input.last)
    val times =  findNumbersIn(input.head)
    (distances zip times).map { case (dist, time) => Race(dist, time) }.toSeq
  }
}
