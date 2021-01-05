import scala.io.Source

package Days
{
  case class Day14()
  {
    def getInput(): Iterator[String] = Source.fromResource("day14.data").getLines

    def part1(): Int = 0

    def part2(): Int = 0

    def execute(): Unit = {
      println();
      println("--- Day 14 ---")
      println(s"Answer to day 14 part 1 is $part1")
      println(s"Answer to day 14 part 2 is $part2")
    }
  }
}