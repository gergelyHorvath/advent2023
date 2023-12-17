package puzzles

import scala.io.Source

object Utils {

  def readInput(fileName: String): Seq[String] = {
    val source = Source.fromFile(s"src/resources/$fileName.txt")
    val lines = source.getLines().toSeq
    source.close()
    lines
  }
}
