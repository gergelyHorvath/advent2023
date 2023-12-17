package puzzles

import scala.annotation.tailrec

object Day3 {
  private val input: Seq[String] = Utils.readInput("day3")

  def main(args: Array[String]): Unit = {
    println(solve1)
    println(solve2)
  }

  final case class Board(symbols: Seq[Symbol], numbers: Seq[Num]) {
    def addSymbol(s: Symbol): Board = this.copy(symbols = symbols :+ s)
    def addNewNumber(n: Num): Board = this.copy(numbers = numbers :+ n)
    def extendNumber(n: Num): Board = {
      val newNumber = Num(numbers.last.value + n.value, numbers.last.coords ++ n.coords)
      this.copy(numbers = numbers.init :+ newNumber)
    }
  }
  final case class Symbol(value: String, coord: (Int, Int))
  final case class Num(value: String, coords: Seq[(Int, Int)])

  def solve1: Int = {
    val board = createBoard
    board.numbers.filter { n =>
      n.coords.exists( c => board.symbols.exists(s => isNear(s.coord, c)))
    }.map(_.value.toInt).sum
  }

  def solve2: Int = {
    val board = createBoard
    board.symbols.filter(_.value == "*").map { s=>
      val nearNums = board.numbers.filter(_.coords.exists(c => isNear(c, s.coord)))
      if (nearNums.size == 2)  nearNums.map(_.value.toInt).product else 0
    }.sum
  }

  private def createBoard: Board = input
    .zipWithIndex
    .map { case (row, rowIdx) => rowToBoard(rowIdx, row.zipWithIndex, Board(Seq.empty, Seq.empty)) }
    .reduce[Board]((b1, b2) => Board(b1.symbols ++ b2.symbols, b1.numbers ++ b2.numbers))

  private def isNear(c1: (Int, Int), c2: (Int, Int)): Boolean =
    Math.abs(c1._1 - c2._1) <= 1 && Math.abs(c1._2 - c2._2) <= 1

  @tailrec
  def rowToBoard(rowIdx: Int, row: Seq[(Char, Int)], board: Board, prev: Char = '.'): Board = {
    val x = rowIdx
    val (current, y) = row.head
    val newBoard = {
      if (current.isDigit) {
        val newNum = Num(current.toString, Seq((x, y)))
        if (prev.isDigit) board.extendNumber(newNum) else board.addNewNumber(newNum)
      } else if (current == '.') board
      else board.addSymbol(Symbol(current.toString, (x, y)))
    }
    if (row.tail.nonEmpty) rowToBoard(x, row.tail, newBoard, current) else newBoard
  }
}
