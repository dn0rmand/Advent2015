package Days

import scala.io.Source
import scala.util.parsing.json.{JSON, JSONObject, JSONArray}

case class Day12()
{
  def getInput(): String = Source.fromResource("day12.data").mkString

  def hasRed(obj: Map[_, _]): Boolean =
    obj.values exists { 
      case v: String => v == "red" 
      case _ => false
    }

  def part1(json: Any): Int = 
    json match {
      case value: Int 
        => value
      case value: Double 
        => value.toInt
      case value: Map[_, _]
        => value.values.map(v => part1(v)).sum
      case value: List[Any]
        => value.map(v => part1(v)).sum
      case _ 
        => 0
    }

  def part2(json: Any): Int =
    json match {
      case value: Int 
        => value
      case value: Double 
        => value.toInt
      case value: Map[_, _]
        =>  if (hasRed(value)) 0 else value.values.map(v => part2(v)).sum
      case value: List[Any] 
        => value.map(v => part2(v)).sum
      case _ 
        => 0
    }

  def execute(): Unit = {
    println();
    println("--- Day 12 ---")

    val json = JSON.parseFull(getInput)

    println(s"Answer to day 12 part 1 is ${part1(json.get)}")
    println(s"Answer to day 12 part 2 is ${part2(json.get)}")
  }
}
