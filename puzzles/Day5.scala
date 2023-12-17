package puzzles

import scala.annotation.tailrec

object Day5 {
  type Mapping = Map[(BigInt, BigInt), BigInt]
  def main(args: Array[String]): Unit = {
//    println(solveQ1)
    solveQ2()
//    println(solveQ2)
  }

  private val input: Seq[String] = Utils.readInput("day5")
  private val seeds: Seq[BigInt] = input.head.split(": ").last.split(" ").toSeq.map(BigInt(_))
  private val seedIntervals: Seq[(BigInt, BigInt)] = seeds.grouped(2).map(x => (x.head, x.head + x.last)).toSeq
  private val maps = createMaps(input)
  private val revMaps = createMaps(input).reverse

  def solveQ1: BigInt = seeds.map(s => pushThroughMaps(s, maps)).min
  //not proud of this one but it is what it is
  def solveQ2(x: BigInt= 0): BigInt = {
    println(x)
    val res = pushThroughMaps(x, revMaps)
    if (seedIntervals.exists(si =>si._1 <= res && res <= si._2)) res else solveQ2(x + 1)
  }
  //  def solveQ2: BigInt = pushThroughMaps(BigInt("2464432461"), maps)

  @tailrec
  private def createMaps(rows: Seq[String], maps: Seq[Mapping] = Seq.empty): Seq[Mapping] =
    if (rows.isEmpty) maps
    else {
      val (current, rest)= rows.dropWhile(!_.headOption.exists(_.isDigit)).span(_.headOption.exists(_.isDigit))
      createMaps(rest, maps :+ parseMap(current))
    }

  @tailrec
  private def pushThroughMaps(value: BigInt, maps: Seq[Mapping]): BigInt = {
//    println(value)
    if (maps.isEmpty) value
    else {
      val newValue: BigInt = maps.head.find { case ((source, interval), _) => source <= value && value <= source + interval }
        .map { case ((source, _), destination) => value + destination - source }
        .getOrElse(value)
      pushThroughMaps(newValue, maps.tail)
    }
  }

  private def parseMap(mapString: Seq[String]): Mapping = {
    mapString.map(_.split(" ").map(BigInt(_)).toList match {
//      case destination :: source :: interval :: _ => (source, interval) -> destination
      case destination :: source :: interval :: _ => (destination, interval) -> source
    }).toMap
  }


}
