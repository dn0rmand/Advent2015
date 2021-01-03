import scala.io.Source

package Days
{
  case class Day3()
  {
    def getInput() : List[Char] = 
      Source.fromResource("day3.data")
            .getLines
            .take(1)
            .toList
            .head
            .toList

    def getStops(input: List[Char]) : Iterable[(Int, Int)] = {
      var (x, y) = (0, 0)

      for(c <- input.prepended('='))
        yield c match {
          case '=' => (x, y)
          case '<' => x -= 1 ; (x, y)
          case '>' => x += 1 ; (x, y)
          case '^' => y -= 1 ; (x, y)
          case 'v' => y += 1 ; (x, y)
          case _ => throw new RuntimeException(s"$c is not a valid command") 
        }
    }

    def getStops2(input: List[Char]) : Iterable[(Int, Int)] = {
      var turn = 1
      val (list1, list2) = input.partition(p => { turn = (turn+1) % 2 ; turn == 0 });

      getStops(list1) ++ getStops(list2)
    }

    def part1(): Int = getStops(getInput).toList.distinct.length

    def part2(): Int = getStops2(getInput).toList.distinct.length

    def execute(): Unit = {
      println();
      println("--- Day 3 ---")
      println(s"Answer to day 3 part 1 is $part1")
      println(s"Answer to day 3 part 2 is $part2")
    }
  }
}