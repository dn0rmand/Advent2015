import scala.io.Source
import scala.util.matching.Regex

package Days
{
  case class Day5()
  {
    def getInput(): Iterator[String] = Source.fromResource("day5.data").getLines

    val aeiou = "aeiou"
    val badPattern: Regex = "(ab)|(cd)|(pq)|(xy)".r

    val goodPattern1: Regex = "(.)\\1".r

    val goodPattern2a: Regex = "(.)(.).*\\1\\2".r
    val goodPattern2b: Regex = "(.).\\1".r

    def vowels(input: String): Int = input.count(c => aeiou.indexOf(c) >= 0)

    def isBad(input: String): Boolean = 
      badPattern.findFirstMatchIn(input) match {
        case Some(_) => true
        case None => false
      }

    def isGood1(input: String): Boolean = 
      goodPattern1.findFirstMatchIn(input) match {
        case Some(_) => true
        case None => false
      }

    def isGood2a(input: String): Boolean = 
      goodPattern2a.findFirstMatchIn(input) match {
        case Some(_) => true
        case None => false
      }

    def isGood2b(input: String): Boolean = 
      goodPattern2b.findFirstMatchIn(input) match {
        case Some(_) => true
        case None => false
      }

    def checkLine1(line: String): Boolean = (vowels(line) >= 3 && isGood1(line) && !isBad(line))
    def checkLine2(line: String): Boolean = (isGood2a(line) && isGood2b(line))

    def part1(): Int = getInput().map[Int](line => if (checkLine1(line)) 1 else 0).sum

    def part2(): Int = getInput().map[Int](line => if (checkLine2(line)) 1 else 0).sum

    def execute(): Unit = {
      println();
      println("--- Day 5 ---")
      println(s"Answer to day 5 part 1 is $part1")
      println(s"Answer to day 5 part 2 is $part2")
    }
  }
}