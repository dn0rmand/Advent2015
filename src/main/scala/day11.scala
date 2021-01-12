package Days

import scala.io.Source
import scala.util.matching.Regex

case class Day11()
{
  def getInput(): String = Source.fromResource("day11.data").getLines.take(1).toList.head
  
  def nextChar(c : Char) : Char = (c+1).toChar
  
  def rule1(password: String) : Boolean = {
    val (_, count) = password.foldLeft((Char.MinValue, 0)) {
      case ((c1, count), c) if (c == c1) => (Char.MinValue, count+1)
      case ((_, count), c) => (c, count)
    }
    
    count > 1
  }

  def rule2(password: String) : Boolean = {
    val (good) = password.foldLeft(true) {
      case (a, c) if (c == 'i' || c == 'o' || c == 'l') => false
      case (a, _) => a
    }
    good
  }

  def rule3(password: String) : Boolean = 
  {
    val (_, _, found) = password.foldLeft((Char.MinValue, Char.MinValue, false)) {
      case ((c1, c2, found), c3) if (c2 == nextChar(c1) && c3 == nextChar(c2)) => (c2, c3, true)
      case ((c1, c2, found), c3) => (c2, c3, found)
    }
    found
  }

  def isValid(password: String) : Boolean = rule1(password) && rule2(password) && rule3(password)

  def nextPassword2(password: String) : String = {
    val (_, pwd) = password.foldRight((1, "")) {
      case (c, (offset, pwd)) if (c == 'z' && offset == 1) => (1, 'a' + pwd)
      case (c, (offset, pwd)) if (offset == 1 && c == 'h') => (0, 'j'+pwd) // skip i
      case (c, (offset, pwd)) if (offset == 1 && c == 'k') => (0, 'm'+pwd) // skip l
      case (c, (offset, pwd)) if (offset == 1 && c == 'n') => (0, 'p'+pwd) // skip o
      case (c, (offset, pwd)) if (offset == 1) => (0, nextChar(c)+pwd)
      case (c, (_, pwd)) => (0, c + pwd) 
    }

    pwd
  }

  def nextPassword(password: String) : String = {
    password indexOf 'i'  match {
      case v if v >= 0 => password.substring(0, v-1) + 'j' + "a"*(password.length-v)
      case _ => password indexOf 'o' match {
        case v if v >= 0 => password.substring(0, v-1) + 'p' + "a"*(password.length-v)
        case _ => password indexOf 'l' match {
          case v if v >= 0 => password.substring(0, v-1) + 'm' + "a"*(password.length-v)
          case _ => nextPassword2(password)
        }
      }
    }
  } 

  def nextValidPassword(pwd: String) : String =
  {
    var password = nextPassword(pwd)

    while (!isValid(password))
      password = nextPassword2(password)

    password
  }

  def part1(): String = nextValidPassword(getInput)

  def part2(pwd: String): String = nextValidPassword(pwd)

  def execute(): Unit = {
    println();
    println("--- Day 11 ---")
    val password = part1
    println(s"Answer to day 11 part 1 is $password")
    println(s"Answer to day 11 part 2 is ${part2(password)}")
  }
}
