package Days

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Day20()
{
  val MAX_HOUSE = 1000000

  def getInput(): Int = Source.fromResource("day20.data").getLines.toArray.head.toInt

  def part1(): Int = 
  {
    val maxGifts = getInput
    var minHouse = MAX_HOUSE// to avoid using too much memory
    val houses = ArrayBuffer.fill(MAX_HOUSE+1) { 10 }

    for(elf <- (2 until minHouse)) {
      var droppedGifts = elf * 10
      for(house <- Range(elf, minHouse, elf)) {
        val gifts = houses(house) + droppedGifts
        houses(house) = gifts
        if (gifts >= maxGifts)
          minHouse = house
      }
    }

    minHouse
  }

  def part2(): Int =
  {
    val maxGifts = getInput
    var minHouse = MAX_HOUSE // to avoid using too much memory
    val houses   = ArrayBuffer.fill(minHouse+1) { 0 }

    for(elf <- (1 until minHouse)) 
    {
      var droppedGifts = elf * 11
      for(house <- Range(elf, scala.math.min(minHouse, 50 * elf), elf))
      {
        val gifts = houses(house) + droppedGifts
        houses(house) = gifts
        if (gifts >= maxGifts)
          minHouse = house
      }
    }

    minHouse
  }

  def execute(): Unit = {
    println();
    println("--- Day 20 ---")
    println(s"Answer to day 20 part 1 is $part1")
    println(s"Answer to day 20 part 2 is $part2")
  }
}
