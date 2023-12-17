package puzzles

import scala.annotation.tailrec

object Day8 {
  private val input: Seq[String] = Utils.readInput("day8")
  private val mapping: Map[String, Seq[String]] = parseMap(input.drop(2))
  private val allSteps: Seq[Int] = input.head.map(c => if (c == 'L') 0 else 1)

  def main(args: Array[String]): Unit = {
    println(solveQ1("AAA"))
    val cycle = allSteps.size
    println(mapping.keys.filter(_.endsWith("A")))
    println(mapping.keys.filter(_.endsWith("Z")))
    // solving all ending with A and Z shows they all repeat on full cycles and are all prime numbers, so LCM = product
    val resQ2 = Seq(18113, 22411, 13201, 18727, 20569, 16271).map(BigInt(_) / cycle).product * cycle
    println(resQ2)

  }

  def solveQ1(start: String): Int =
    simulate(allSteps, start, 0)

  @tailrec
  private def simulate(steps: Seq[Int], current: String, counter: Int = 0): Int =
    if (current.endsWith("Z") && counter > 0) counter
    else {
      val newSteps = if (steps.isEmpty) allSteps else steps
      simulate (newSteps.tail, mapping(current)(newSteps.head), counter + 1)
    }

  private def parseMap(input: Seq[String]): Map[String, Seq[String]] =
    input.map { row =>
      "[A-Z]{3}".r.findAllIn(row).toList match {
        case key :: left :: right :: _ =>  key -> Seq(left, right)
      }
    }.toMap
}
