package Days

import scala.io.Source

case class Day25()
{
  val parser = "To continue, please consult the code grid in the manual.  Enter the code at row (.*), column (.*).".r
  def getInput(): (Int, Int) = 
    Source.fromResource("day25.data")
          .getLines.toArray.head match { case parser(row, column) => (row.toInt, column.toInt) }

  def get(column: Int, row: Int) : Long = {
    var x = 1
    var y = 1
    var value: Long = 20151125
    var yy = 1;

    while(x != column || y != row) {
      if (y == 1) {
        yy += 1
        x = 1
        y = yy
      }
      else {
        x += 1
        y -= 1
      }
      value = (value * 252533) % 33554393
    }

    value
  }

  def part1(): Long = getInput() match { case (row: Int, column: Int) => get(column, row) }

  def part2(): String = "Well Done! You got all the 25 starts"

  def execute(): Unit = {
    println();
    println("--- Day 25 ---")
    println(s"Answer to day 25 part 1 is $part1")
    println(s"Answer to day 25 part 2 is $part2")
  }
}
