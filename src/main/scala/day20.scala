import scala.io.Source

package Days
{
  case class Day20()
  {
    def getInput(): Iterator[String] = Source.fromResource("day20.data").getLines

    def part1(): Int = 0

    def part2(): Int = 0

    def execute(): Unit = {
      println();
      println("--- Day 20 ---")
      println(s"Answer to day 20 part 1 is $part1")
      println(s"Answer to day 20 part 2 is $part2")
    }
  }
}