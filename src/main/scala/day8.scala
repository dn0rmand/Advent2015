import scala.io.Source
import scala.util.matching.Regex

package Days
{
  case class Day8()
  {
    def getInput(): Iterator[String] = Source.fromResource("day8.data").getLines

    def unescapedLength(line: String): Int = {
      var skip = 0
      var length = -2

      for(i <- (0 until line.length))
        if (skip > 0) skip -= 1 else line(i) match {
          case '\\' => line(i+1) match {
            case 'x' => skip = 3; length += 1
            case _ => skip = 1; length += 1
          }
          case _ => length += 1
        }

      return length
    }

    def escapeLength(line: String): Int = {
      var skip = 0
      var length = 2

      for(i <- (0 until line.length))
        line(i) match {
          case '\\' => length += 2
          case '"' => length += 2
          case _ => length += 1
        }

      return length
    }

    def part1(): Int =
        getInput().map(line => line.length - unescapedLength(line)).sum

    def part2(): Int =
        getInput().map(line => escapeLength(line) - line.length).sum

    def execute(): Unit = {
      println();
      println("--- Day 8 ---")
      println(s"Answer to day 8 part 1 is $part1")
      println(s"Answer to day 8 part 2 is $part2")
    }
  }
}