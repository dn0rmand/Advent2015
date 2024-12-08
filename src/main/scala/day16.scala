package Days

import scala.io.Source

case class Day16()
{
  val criteria = Map(
    "children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "akitas" -> 0, 
    "vizslas" -> 0, "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1
  )

  val greaterKeys = List("cats", "trees")
  val lesserKeys = List("pomeranians", "goldfish")

  case class Sue(val id: Int, val properties: Map[String, Int])
  {
    def isExactMatch : Boolean =
      properties.foldLeft(true) { 
        case (a, (key, value)) => a && (!(criteria contains key) || criteria(key) == value)
      }

    def isOkValue(key: String, value: Int) : Boolean = 
      if (criteria contains key) {
        if (greaterKeys contains key) {
          value > criteria(key)
        } else if (lesserKeys contains key) {
          value < criteria(key)
        } else {
          value == criteria(key)
        }
      }
      else {
        true
      }
      
    def isSmartMatch : Boolean = properties.foldLeft(true) { case (a, (key, value)) => a && isOkValue(key, value) }
  }

  // Sue 295: goldfish: 0, akitas: 9, cats: 0

  val parser = "Sue (.*): (.*): (.*), (.*): (.*), (.*): (.*)".r

  def getInput(): Array[Sue] =
    Source.fromResource("day16.data")
          .getLines
          .map({
            case parser(id, key1, value1, key2, value2, key3, value3) 
              => Sue(id.toInt, Map(key1 -> value1.toInt, key2 -> value2.toInt, key3 -> value3.toInt))
            case _ 
              => throw new RuntimeException("Invalid data")
          })
          .toArray

  def part1(): Int = getInput.filter(sue => sue.isExactMatch).head.id

  def part2(): Int = getInput.filter(sue => sue.isSmartMatch).head.id

  def execute(): Unit = {
    println();
    println("--- Day 16 ---")
    println(s"Answer to day 16 part 1 is $part1")
    println(s"Answer to day 16 part 2 is $part2")
  }
}
