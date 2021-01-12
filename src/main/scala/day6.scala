package Days

import scala.io.Source

case class Day6()
{
  val WIDTH = 1000
  val HEIGHT= 1000

  def getInput(): Iterator[(String, Int, Int, Int, Int)] = 
      Source.fromResource("day6.data")
            .getLines
            .map(line => {
              var entry = line.replace(" through ", ",").replace("turn ", "").replace(" ", ",").split(",")
              ( entry(0), entry(1).toInt, entry(2).toInt, entry(3).toInt, entry(4).toInt )
            })

  def part1(): Int = {

    var img = new Array[Int](WIDTH*HEIGHT)

    for((op, x1, y1, x2, y2) <- getInput) {
      
      op match {
        case "on"  => for (x <- (x1 to x2); y <- (y1 to y2)) {
          img(x + y*WIDTH) = 1
        }

        case "off" => for (x <- (x1 to x2); y <- (y1 to y2)) {
          img(x + y*WIDTH) = 0
        }

        case "toggle" => for (x <- (x1 to x2); y <- (y1 to y2)) {
          img(x + y*WIDTH) = img(x + y*WIDTH) ^ 1
        }
      }
    }

    img.sum
  }

  def part2(): Int = {

    var img = new Array[Int](WIDTH*HEIGHT)

    for((op, x1, y1, x2, y2) <- getInput) {
      op match {
        case "on"  => for (x <- (x1 to x2); y <- (y1 to y2)) {
          img(x + y*WIDTH) = img(x + y*WIDTH) + 1
        }

        case "off" => for (x <- (x1 to x2); y <- (y1 to y2)) {
          if (img(x + y*WIDTH) > 0) img(x + y*WIDTH) = img(x + y*WIDTH) - 1
        }

        case "toggle" => for (x <- (x1 to x2); y <- (y1 to y2)) {
          img(x + y*WIDTH) = img(x + y*WIDTH) + 2
        }
      }
    }

    img.sum
  }

  def execute(): Unit = {
    println();
    println("--- Day 6 ---")
    println(s"Answer to day 6 part 1 is $part1")
    println(s"Answer to day 6 part 2 is $part2")
  }
}
