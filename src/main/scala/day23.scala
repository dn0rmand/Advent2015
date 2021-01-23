package Days

import scala.io.Source

case class Day23()
{
  abstract class Instruction
  {
    def execute(context: Context) : Context
  }

  case class JioInstruction(register: String, offset: Int) extends Instruction
  {
    override def execute(context: Context): Context =
      if (context(register) == 1) 
        Context(context.a, context.b, context.ip + offset) 
      else 
        Context(context.a, context.b, context.ip + 1) 
  }

  case class JieInstruction(register: String, offset: Int) extends Instruction
  {
    override def execute(context: Context): Context = 
      if (context(register) % 2 == 0) 
        Context(context.a, context.b, context.ip + offset) 
      else 
        Context(context.a, context.b, context.ip + 1) 
  }

  case class JmpInstruction(offset: Int) extends Instruction
  {
    override def execute(context: Context): Context = 
      Context(context.a, context.b, context.ip + offset)
  }

  case class IncInstruction(register: String) extends Instruction
  {
    if (register != "a" && register != "b")
      throw new RuntimeException("Invalid Register")

    override def execute(context: Context): Context = 
      if (register == "a")
        Context(context.a + 1, context.b, context.ip + 1)
      else
        Context(context.a, context.b+1, context.ip + 1)

  }

  case class HlfInstruction(register: String) extends Instruction
  {
    if (register != "a" && register != "b")
      throw new RuntimeException("Invalid Register")

    override def execute(context: Context): Context = 
      if (register == "a")
        Context(context.a / 2, context.b, context.ip + 1)
      else
        Context(context.a, context.b / 2, context.ip + 1)
  }

  case class TplInstruction(register: String) extends Instruction
  {
    if (register != "a" && register != "b")
      throw new RuntimeException("Invalid Register")

    override def execute(context: Context): Context = 
      if (register == "a")
        Context(context.a * 3, context.b, context.ip + 1)
      else
        Context(context.a, context.b * 3, context.ip + 1)
  }

  case class Context(val a: Long = 0, val b: Long = 0, val ip: Int = 0)
  {
    if (a < 0 || b < 0)
      throw new RuntimeException("Overflow error");

    def apply(register: String) : Long =
      register match {
        case "a" => a
        case "b" => b
        case _ => throw new RuntimeException("Invalid register")
      }
  }

  case class Computer(instructions: Array[Instruction])
  {
    def <(context: Context) : Context = {
      if (context.ip < 0 || context.ip >= instructions.length)
        Context(context.a, context.b, -1)
      else
        instructions(context.ip).execute(context)
    }
  }

  val parserJio = "jio ([a|b]), (.*)".r
  val parserJie = "jie ([a|b]), (.*)".r
  val parserInc = "inc ([a|b])".r
  val parserHlf = "hlf ([a|b])".r
  val parserTpl = "tpl ([a|b])".r
  val parserJmp = "jmp (.*)".r

  def getInput(): Array[Instruction] = 
    Source.fromResource("day23.data")
          .getLines
          .map(line => {
            line match {
              case parserJio(register, offset) => JioInstruction(register, offset.toInt)
              case parserJie(register, offset) => JieInstruction(register, offset.toInt)
              case parserJmp(offset) => JmpInstruction(offset.toInt)
              case parserInc(register) => IncInstruction(register)
              case parserHlf(register) => HlfInstruction(register)
              case parserTpl(register) => TplInstruction(register)
              case _ => throw new RuntimeException("Invalid instruction")
            }
          })
          .toArray

  def part1(): Long = {
    val computer = Computer(getInput)
    var context  = Context()

    while(context.ip >= 0)
    {
      context = computer < context
    }

    context.b
  }

  def part2(): Long = {
    val computer = Computer(getInput)
    var context  = Context(a=1)

    while(context.ip >= 0)
    {
      context = computer < context
    }

    context.b
  }

  def execute(): Unit = {
    println();
    println("--- Day 23 ---")
    println(s"Answer to day 23 part 1 is $part1")
    println(s"Answer to day 23 part 2 is $part2")
  }
}
