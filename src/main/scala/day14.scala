package Days

import scala.io.Source

case class Day14()
{
  case class Deer(name: String, speed: Int, travelTime: Int, restTime: Int)
  {
    private val totalTime = travelTime + restTime

    var points : Int = 0
    var traveled: Int = 0
    var time: Int = 0

    def distance(time: Int) : Int = {
      val remain = time % totalTime
      val firstPart = ((time - remain) / totalTime) * (speed * travelTime)
      val secondPart = if (remain < travelTime) remain*speed else travelTime*speed

      return firstPart + secondPart
    }

    def move() : Int = {
      if (time < travelTime)
        traveled += speed

      time = (time + 1) % totalTime 

      return traveled
    } 
  }

  def getInput(): Iterator[Deer] = {
    val info = Source.fromResource("day14.data")
          .getLines
          .map(line => {
            val data = line.replace(" can fly ", ",")
                           .replace(" km/s for ", ",")
                           .replace(" seconds, but then must rest for ", ",")
                           .replace(" seconds.", "")
                           .split(",")
            Deer(data(0), data(1).toInt, data(2).toInt, data(3).toInt)
          })
    return info
  }

  def part1(): Int = getInput.map(deer => deer.distance(2503)).max

  def part2(): Int = {
    val deers = getInput.toArray

    for(i <- (1 to 2503)) {
      val maxDistance = deers.map(_.move).max
      deers.filter(_.traveled == maxDistance)
           .foreach(_.points += 1)
    }

    deers.map(_.points).max
  }

  def execute(): Unit = {
    println();
    println("--- Day 14 ---")
    println(s"Answer to day 14 part 1 is $part1")
    println(s"Answer to day 14 part 2 is $part2")
  }
}
