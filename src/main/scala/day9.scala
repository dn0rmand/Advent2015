import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set}

package Days
{
  case class Day9()
  {
    type Graph = Map[String, Map[String, Int]]

    def getInput(): Graph = {
      val reg : Regex = "( to )|( = )".r
      var graph : Graph = Map("dummy" -> Map("dummy" -> 1))
      
      graph -= "dummy"

      Source.fromResource("day9.data").getLines.toList.map(line => {
              val entries = reg.replaceAllIn(line, ",").split(",")
              val v1 = entries(0)
              val v2 = entries(1)
              val distance = entries(2).toInt

              if (graph contains v1) graph(v1) += v2 -> distance else graph += (v1 -> Map(v2 -> distance))
              if (graph contains v2) graph(v2) += v1 -> distance else graph += (v2 -> Map(v1 -> distance))
            })

        graph
    }

    def shortest(graph: Graph, current: String, visited: Set[String]): Int = {
      val notVisited = graph(current).filterNot(node => visited contains node._1)
      if (notVisited.size == 0)
        0
      else
        notVisited.map(node => node._2 + shortest(graph, node._1, visited + node._1))
                  .min
    }

    def longest(graph: Graph, current: String, visited: Set[String]): Int = {
      val notVisited = graph(current).filterNot(node => visited contains node._1)
      if (notVisited.size == 0)
        0
      else
        notVisited.map(node => node._2 + longest(graph, node._1, visited + node._1))
                  .max
    }

    def part1(): Int = {
      val graph = getInput
      graph.map(node => shortest(graph, node._1, Set(node._1))).min
    }

    def part2(): Int = {
      val graph = getInput
      graph.map(node => longest(graph, node._1, Set(node._1))).max
    }

    def execute(): Unit = {
      println();
      println("--- Day 9 ---")
      println(s"Answer to day 9 part 1 is $part1")
      println(s"Answer to day 9 part 2 is $part2")
    }
  }
}