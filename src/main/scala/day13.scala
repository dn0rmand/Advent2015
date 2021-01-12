package Days

import scala.io.Source

case class Day13()
{
  def getInput(): Iterator[String] = Source.fromResource("day13.data").getLines

  def part1(): Int = 0

  def part2(): Int = 0

  def execute(): Unit = {
    println();
    println("--- Day 13 ---")
    println(s"Answer to day 13 part 1 is $part1")
    println(s"Answer to day 13 part 2 is $part2")
  }
}
