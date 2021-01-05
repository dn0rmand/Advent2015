import scala.io.Source

package Days
{
  case class Day25()
  {
    def getInput(): Iterator[String] = Source.fromResource("day25.data").getLines

    def part1(): Int = 0

    def part2(): Int = 0

    def execute(): Unit = {
      println();
      println("--- Day 25 ---")
      println(s"Answer to day 25 part 1 is $part1")
      println(s"Answer to day 25 part 2 is $part2")
    }
  }
}