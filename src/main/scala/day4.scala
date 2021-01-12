package Days

import scala.io.Source
import java.security.MessageDigest
import java.math.BigInteger

case class Day4()
{
  val prefix : String = 
    Source.fromResource("day4.data")
          .getLines
          .take(1)
          .toList
          .head

  val md5 = MessageDigest.getInstance("MD5")

  def getHash(value: Int) : Array[Byte] = md5.digest((prefix + value.toString()).getBytes)

  def part1(): Int = Range(0, Int.MaxValue).dropWhile(x => {
      val hash = getHash(x) 
      hash(0) != 0 || hash(1) != 0 || hash(2) < 0 || hash(2) > 15
    }).take(1).end

  def part2(): Int = 
    Range(0, Int.MaxValue).dropWhile(x => {
      val hash = getHash(x) 
      hash(0) != 0 || hash(1) != 0 || hash(2) != 0
    }).take(1).end

  def execute(): Unit = {
    println();
    println("--- Day 4 ---")
    println(s"Answer to day 4 part 1 is $part1")
    println(s"Answer to day 4 part 2 is $part2")
  }
}