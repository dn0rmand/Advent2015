import scala.io.Source

package Days
{
  case class Day11()
  {
    def getInput(): Iterator[String] = Source.fromResource("day11.data").getLines

    def part1(): Int = 0

    def part2(): Int = 0

    def execute(): Unit = {
      println();
      println("--- Day 11 ---")
      println(s"Answer to day 11 part 1 is $part1")
      println(s"Answer to day 11 part 2 is $part2")
    }
  }
}