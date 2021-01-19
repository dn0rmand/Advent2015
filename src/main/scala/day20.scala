package Days

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

case class Day20()
{
  def getInput(): Int = Source.fromResource("day20.data").getLines.toArray.head.toInt

  def divisors(n: Int): Array[Int] = {
    var divs = if (n == 1) Array(1) else Array(1, n)
    var max  = n 
    var min  = 2

    while (min < max) 
    {
      if ((n % min) == 0) {
        max = n / min
        divs = divs :+ min
        if (max > min) divs = divs :+ max
      }
      min += 1
    }

    divs.sortWith((a, b) => a < b);
  }

  def getHouseGiftCount(house: Int): Int =
  {
    var gifts = 0
    for(elf <- divisors(house)) {
      gifts += elf * 10
    }
    gifts
  }

  def part1(): Int = 
  {
    val maxGifts = getInput
    var minHouse = maxGifts / 10 // to avoid using too much memory
    val houses = ArrayBuffer.fill(minHouse+1) { 10 }

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
    var minHouse = maxGifts / 10 // to avoid using too much memory
    val houses   = ArrayBuffer.fill(minHouse+1) { 11 }

    for(elf <- (2 until minHouse)) 
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
