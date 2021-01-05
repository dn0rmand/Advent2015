import scala.io.Source

package Days
{
  case class Day9()
  {
    def getInput(): Iterator[String] = Source.fromResource("day9.data").getLines

    def part1(): Int = 0

    def part2(): Int = 0

    def execute(): Unit = {
      println();
      println("--- Day 9 ---")
      println(s"Answer to day 9 part 1 is $part1")
      println(s"Answer to day 9 part 2 is $part2")
    }
  }
}