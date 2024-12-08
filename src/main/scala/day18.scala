package Days

import scala.io.Source

case class Day18()
{
  val WIDTH  = 100
  val HEIGHT = 100

  case class Lights(lights: Array[Int], part2: Boolean) 
  {       
    def get(x: Int, y: Int) : Int = {
      if (part2)
      {
        if (x == 0 && y == 0) return 1
        if (x == 0 && y == HEIGHT-1) return 1
        if (x == WIDTH-1 && y == 0) return 1
        if (x == WIDTH-1 && y == HEIGHT-1) return 1 
      }

      if (x < 0 || x >= WIDTH || y < 0 || y >= HEIGHT)
        0
      else
        lights(x + y*WIDTH)
    }

    def processLight(x: Int, y: Int) : Int =
    {
      val current = get(x, y);

      val others  = for { xx <- x-1 to x+1; yy <- y-1 to y+1 } yield get(xx, yy)
      val count = others.sum - current

      if (current == 1)
        if (count == 2 || count == 3) 1 else 0
      else
        if (count == 3) 1 else 0
    }

    def next() : Lights = {
      val nextLigths = for(i <- (0 until lights.length)) yield processLight(i % WIDTH, (i - i % WIDTH) / WIDTH)
      
      Lights(nextLigths.toArray, part2);
    }

    def sum() : Int = {
      if (part2)
        lights.sum + 4 - lights(0) - lights(WIDTH-1) - lights(WIDTH*HEIGHT-WIDTH) - lights(WIDTH*HEIGHT-1)
      else
        lights.sum
    }
  }

  def getInput(part2: Boolean = false): Lights = 
      Lights(
        Source.fromResource("day18.data")
              .getLines
              .map(line => line.toArray.map(c => if (c == '#') 1 else 0))
              .flatten
              .toArray
        ,
        part2
      )

  def part1(): Int = {
    var lights = getInput(false);

    for(i <- 1 to 100) lights = lights.next
    lights.sum
  }

  def part2(): Int = {
    var lights = getInput(true);

    for(i <- 1 to 100) lights = lights.next
    lights.sum
  }

  def execute(): Unit = {
    println();
    println("--- Day 18 ---")
    println(s"Answer to day 18 part 1 is $part1")
    println(s"Answer to day 18 part 2 is $part2")
  }
}
