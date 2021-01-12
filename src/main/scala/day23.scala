package Days

import scala.io.Source

case class Day23()
{
  def getInput(): Iterator[String] = Source.fromResource("day23.data").getLines

  def part1(): Int = 0

  def part2(): Int = 0

  def execute(): Unit = {
    println();
    println("--- Day 23 ---")
    println(s"Answer to day 23 part 1 is $part1")
    println(s"Answer to day 23 part 2 is $part2")
  }
}
