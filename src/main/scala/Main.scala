import Days._


object Main extends App {

  def time[R](block: => R): R = {
    val start = System.currentTimeMillis()
    val result = block
    val end = System.currentTimeMillis() - start
    var ms = end % 1000 
    var seconds = (end-ms) / 1000

    println(s"Elapsed time: $seconds secs ${ms} ms")
    result
  }

  println("################################")
  println("# Advent of Code 2015 in Scala #")
  println("################################")
  
  time {
    Day1().execute
    Day2().execute
    Day3().execute
    Day4().execute
    Day5().execute
    Day6().execute
    Day7().execute
    Day8().execute
    Day9().execute
    Day10().execute
    Day11().execute
    Day12().execute
    Day13().execute
    Day14().execute
    Day15().execute
    Day16().execute
    Day17().execute
    Day18().execute
    Day19().execute
    Day20().execute
    Day21().execute
    Day22().execute
    Day23().execute
    Day24().execute
    Day25().execute
    println()
    println("Done")
  }
}