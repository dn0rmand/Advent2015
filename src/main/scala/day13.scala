package Days

import scala.io.Source

case class Persons(persons: Array[(String, String, Int)])
{
  private val people  = (persons.map(p => p._1) ++ persons.map(p => p._2)).distinct

  private def happiness(p1: String, p2: String) : Int =
      persons.filter(p => (p._1 == p1 && p._2 == p2) || (p._1 == p2 && p._2 == p1))
             .map(p => p._3)
             .sum

  private def calculate(current: String, seated: List[String]) : Int = 
  {
    val targets = persons.filter(f => f._1 == current && !seated.contains(f._2))

    if (targets.size == 0) {
      return happiness(current, seated.head)
    }

    targets.map(p => happiness(p._1, p._2) + calculate(p._2, seated :+ p._2)).max;
  }

  def max() : Int = calculate(persons.head._1, List(persons.head._1))

  def +(me: String) : Persons =
    Persons(people.map(name => (me, name, 0)) ++ people.map(name => (name, me, 0)) ++ persons)
}

case class Day13()
{
  def makeKey(p1: String, p2: String) : (String, String) = if (p1 < p2) (p1, p2) else (p2, p1)
    
  def getInput(): Persons = {
    val input = Source.fromResource("day13.data")
          .getLines
          .map[(String, String, Int)](line => {
            val info = line
                .replace(" happiness units by sitting next to ", ",")
                .replace(" would gain ", ",")
                .replace(" would lose ", ",-")
                .replace(".", "")
                .split(',')
            (info(0), info(2), info(1).toInt)
          })

      Persons(input.toArray)
  }

  def part1(): Int = getInput.max

  def part2(): Int = (getInput + "me").max

  def execute(): Unit = {
    println();
    println("--- Day 13 ---")
    println(s"Answer to day 13 part 1 is $part1")
    println(s"Answer to day 13 part 2 is $part2")
  }
}
 