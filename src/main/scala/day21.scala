package Days

import scala.io.Source

case class Day21()
{
  def getInput(): Iterator[String] = Source.fromResource("day21.data").getLines

  def part1(): Int = 0

  def part2(): Int = 0

  def execute(): Unit = {
    println();
    println("--- Day 21 ---")
    println(s"Answer to day 21 part 1 is $part1")
    println(s"Answer to day 21 part 2 is $part2")
  }
}
