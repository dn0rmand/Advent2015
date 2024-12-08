package Days

import scala.io.Source

case class Day24()
{
  case class Result(val gifts: Array[Long]) 
  {
    val count = if (gifts.length > 0) gifts.length else 1000
    val quantum = if (gifts.length > 0) gifts.product else 0

    def +(weight: Long) : Result = {
      if (weight > 0) Result(gifts :+ weight) else this
    }

    def <(other: Result) : Boolean = 
      count < other.count || (count == other.count && quantum < other.quantum)
  }

  val allWeights: Array[Long] = Source.fromResource("day24.data").getLines.map(_.toLong).toArray.sortWith((a, b) => a > b)
  val totalWeight = allWeights.sum
  var packWeight = allWeights.sum / 3

  val bad = Result(Array())
  var bestSoFar = bad

  def canMakePacks(rest: Array[Long], remain : Long) : Boolean =
  {
    if (remain == 0) true else rest.filter(_ <= remain)
                                   .foldLeft(false)((a, w) => a || canMakePacks(rest.filterNot(_ == w), remain - w))
  }

  def mainPack(used: Array[Long], remaining: Long): Result =
  {  
    if (used.length > bestSoFar.count)
      return bad

    if (used.length == bestSoFar.count && remaining > 0)
      return bad

    val rest = allWeights.filterNot(p => used.exists(_ <= p))

    if (remaining == 0)
    {
      val result = Result(used)
      if (result < bestSoFar) {
        bestSoFar = result
        return result
      }
      else {
        return bad
      }
    }

    rest.filter(_ <= remaining).foldLeft(bad)((a, w) => {
      val r = mainPack(used :+ w, remaining-w)
      if (r < a) r else a 
    })
  }

  def part1(): Long = {
    packWeight = totalWeight / 3
    bestSoFar  = bad
    mainPack(Array(), packWeight).quantum
  }

  def part2(): Long = {
    packWeight = totalWeight / 4
    bestSoFar  = bad
    mainPack(Array(), packWeight).quantum
  }

  def execute(): Unit = {
    println();
    println("--- Day 24 ---")
    println(s"Answer to day 24 part 1 is $part1")
    println(s"Answer to day 24 part 2 is $part2")
  }
}