package Days

import scala.io.Source

case class Day16()
{
  def getInput(): Iterator[String] = Source.fromResource("day16.data").getLines

  def part1(): Int = 0

  def part2(): Int = 0

  def execute(): Unit = {
    println();
    println("--- Day 16 ---")
    println(s"Answer to day 16 part 1 is $part1")
    println(s"Answer to day 16 part 2 is $part2")
  }
}
