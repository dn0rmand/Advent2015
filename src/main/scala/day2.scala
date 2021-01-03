import scala.io.Source

package Days
{
  case class Day2()
  {
    def getInput() : Array[(Int, Int, Int)] =
      Source.fromResource("day2.data")
            .getLines.toArray
            .map(line => {
                val values =  line.split("x")
                                  .map(v => v.toInt)
                                  .sortWith((v1, v2) => v1 < v2)
                (values(0), values(1), values(2))
            })

    def part1(): Int = getInput.map(v => 
        v match {
          case (l, w, h) => l*w + 2*(l*w + w*h + h*l)
        }).sum

    def part2(): Int = getInput.map(v => 
        v match {
          case (l, w, h) => l*w*h + l+l + w+w
        }).sum

    def execute(): Unit = {
      println();
      println("--- Day 2 ---")
      println(s"Answer to day 2 part 1 is $part1")
      println(s"Answer to day 2 part 2 is $part2")
    }
  }
}