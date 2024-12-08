package Days

import scala.io.Source

case class Day19()
{
  case class Factory(items: Map[String, String], start: String)
  {
    private def search(input: String, to: String, from: String) : Iterable[String] = 
    {
      var data = Array[String]()
      var idx = input.indexOf(from)

      while(idx >= 0) {
        data = data :+ input.substring(0, idx) + to + input.substring(idx + from.length)
        idx = input.indexOf(from, idx+1)
      }  

      data.distinct
    }

    def calibrate() : Int =
    {
      var set = Set[String]()

      items foreach { case (to, from) => set = set ++ search(start, to, from) }

      return set.size
    }

    private def decrypt(molecules: Array[String]) : Array[String] =
    {  
      var set = Set[String]()
      molecules.foreach(molecule => items foreach { case (to, from) => set = set ++ search(molecule, from, to) })
      
      val newMolecules = set.toArray.sortWith((s1, s2) => s1.length < s2.length).take(25);

      newMolecules
    }

    def countSteps() : Int = 
    {
      var steps = 0;
      var molecules = Array(start)

      while (molecules.length > 0 && !(molecules contains "e"))
      {
        steps += 1
        molecules = decrypt(molecules)
      }

      return steps
    }
  }

  val parser = "(.*) => (.*)".r
  def getInput(): Factory = {
    val m = Source.fromResource("day19.data")
            .getLines
            .filter(line => line.length != 0)
            .map({
              case parser(from, to) 
                => to -> from
              case v 
                => "*" -> v
            })
            .toMap
    Factory(m - "*", m("*"))
  }

  def part1(): Int = getInput.calibrate

  def part2(): Int = getInput.countSteps

  def execute(): Unit = {
    println();
    println("--- Day 19 ---")
    println(s"Answer to day 19 part 1 is $part1")
    println(s"Answer to day 19 part 2 is $part2")
  }
}
