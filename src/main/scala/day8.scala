import scala.io.Source

package Days
{
  case class Day8()
  {
    def getInput(): Iterator[String] = Source.fromResource("day8.data").getLines

    def part1(): Int = 0

    def part2(): Int = 0

    def execute(): Unit = {
      println();
      println("--- Day 8 ---")
      println(s"Answer to day 8 part 1 is $part1")
      println(s"Answer to day 8 part 2 is $part2")
    }
  }
}