package Days

import scala.io.Source

case class Day22()
{
  def getInput(): Iterator[String] = Source.fromResource("day22.data").getLines

  def part1(): Int = 0

  def part2(): Int = 0

  def execute(): Unit = {
    println();
    println("--- Day 22 ---")
    println(s"Answer to day 22 part 1 is $part1")
    println(s"Answer to day 22 part 2 is $part2")
  }
}
