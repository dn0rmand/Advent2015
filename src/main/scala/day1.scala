import scala.io.Source

object Day1 
{
  case class Day1()
  {
    def getInput() : List[Char] = 
      Source.fromResource("day1.data")
            .getLines
            .take(1)
            .toList
            .head
            .toList
            .filter(c => c == '(' || c == ')')

    def part1(): Int = 
      getInput.map(c => c match { 
        case '(' => 1
        case ')' => -1
      }).sum

    def part2(): Int = 
    {
      var level = 0

      getInput.takeWhile(c => {
        c match {
          case '(' => level += 1
          case ')' => level -= 1
        }
        level != -1
      }).length+1
    }

    def execute(): Unit = {
      println();
      println("--- Day 1 ---")
      println(s"Answer to day 1 part 1 is $part1")
      println(s"Answer to day 1 part 2 is $part2")
    }
  }
}