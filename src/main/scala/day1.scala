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

    def part1(): Unit = 
    {
      var level = 0

      for(c <- getInput) {
        c match {
          case '(' => level += 1
          case ')' => level -= 1
        }
      }
      println(s"Answer to day 1 part 1 is $level")
    }

    def part2(): Unit = 
    {
      var level = 0
      var position = 0

      getInput.takeWhile(c => {
        position += 1
        c match {
          case '(' => level += 1
          case ')' => level -= 1
        }
        level != -1
      })

      println(s"Answer to day 1 part 1 is $position")
    }

    def execute(): Unit = {
      part1
      part2
    }
  }
}