import scala.io.Source

package Days
{
  class Wire(val name: String, val instructions: Array[String])
  {
    val MASK : Int = 65535
    var value: Option[Int] = None

    def getSubValue(v : String, wires: Array[Wire]) : Int = {
      val wire = wires.find(w => w.name == v)
      if (wire.isDefined) wire.get.getValue(wires) else v.toInt
    }

    def calculateValue(wires: Array[Wire]) : Option[Int] = {
      value = Option(instructions.length match {
        case 1 => getSubValue(instructions(0), wires)

        case 2 => getSubValue(instructions(1), wires) ^ MASK // Must be NOT
        
        case 3 => {
          val v1 = getSubValue(instructions(0), wires)
          val v2 = getSubValue(instructions(2), wires)
          instructions(1) match {
            case "AND" => v1 & v2
            case "OR" => v1 | v2
            case "RSHIFT" => v1 >> v2
            case "LSHIFT" => (v1 << v2) & MASK // avoid overflow
            case _ => throw new RuntimeException(s"${instructions(1)} is not valid")
          }
        }
      })
      return value
    }

    def getValue(wires: Array[Wire]) : Int = if (value.isDefined) value.get else calculateValue(wires).get
  }

  case class Day7()
  {
    def getInput(): Array[Wire] = 
        Source.fromResource("day7.data")
              .getLines.map(line => new Wire(line.split(" -> ")(1), line.split(" -> ")(0).split(' ')))
              .toArray

    def part1(): Int = {
      val wires = getInput
      wires.find(w => w.name == "a").get.getValue(wires)
    }

    def part2(bValue: Int): Int = {
      val wires = getInput
      var b = wires.find(w => w.name == "b").get
      b.value = Option(bValue)
      wires.find(w => w.name == "a").get.getValue(wires)
    }

    def execute(): Unit = {
      println();
      println("--- Day 7 ---")
      val p1 = part1
      println(s"Answer to day 7 part 1 is $p1")
      println(s"Answer to day 7 part 2 is ${part2(p1)}")
    }
  }
}