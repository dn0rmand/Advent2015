package Days

import scala.io.Source

case class Day15()
{
  def getInput(): Iterator[String] = Source.fromResource("day15.data").getLines

  def part1(): Int = 0

  def part2(): Int = 0

  def execute(): Unit = {
    println();
    println("--- Day 15 ---")
    println(s"Answer to day 15 part 1 is $part1")
    println(s"Answer to day 15 part 2 is $part2")
  }
}
