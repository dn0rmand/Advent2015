package Days

import scala.io.Source

case class Day17()
{
  def getInput(): Array[Int] = Source.fromResource("day17.data").getLines.map(_.toInt).toArray

  def calc(containers: Array[Int], liters: Int, usedContainers: Int) : List[Int] =
    if (liters == 0)
      List(usedContainers)
    else if (liters < 0 || containers.length == 0)
      List()
    else if (containers(0) > liters)
      calc(containers.drop(1), liters, usedContainers)
    else 
      calc(containers.drop(1), liters, usedContainers) ::: 
      calc(containers.drop(1), liters-containers(0), usedContainers+1)

  def part1(): Int = calc(getInput, 150, 0).length

  def part2(): Int = {
    val possibilities = calc(getInput, 150, 0).sortWith(_ < _)
    possibilities.takeWhile(_ == possibilities(0)).length
  }

  def execute(): Unit = {
    println();
    println("--- Day 17 ---")
    println(s"Answer to day 17 part 1 is $part1")
    println(s"Answer to day 17 part 2 is $part2")
  }
}
