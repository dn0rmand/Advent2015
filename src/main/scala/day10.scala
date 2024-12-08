package Days

import scala.io.Source
import scala.collection.mutable.Queue

trait CharIterator
{
  def hasNext() : Boolean;
  def next() : Char
  def last() : Option[Char]
}

case class DefaultCharIterator(data: Array[Char]) extends CharIterator
{
  val inner = data.iterator
  var _last : Option[Char] = None

  def hasNext() : Boolean = inner.hasNext
  def next() : Char = {
    _last = Option(inner.next())
    _last.get
  }

  def last() : Option[Char] = _last
}

case class LookAndSayIterator(input: CharIterator) extends CharIterator
{
  var queue = Queue[Char]()
  var count = 0
  var _last : Option[Char] = None

  def hasNext() : Boolean = !queue.isEmpty || input.hasNext()

  def last(): Option[Char] = _last

  def next() : Char = {
    if (! queue.isEmpty) {
      _last = Option(queue.dequeue())
      return _last.get
    }

    if (input.hasNext()) {
      val previous = if (input.last != None) input.last.get else input.next()
      var count = 1
      while (input.hasNext() && input.next() == previous) {
        count += 1
      }

      queue ++= count.toString()
      queue += previous

      if (!input.hasNext() && input.last != None && input.last.get != previous)
      {
        queue += '1'
        queue += input.last.get
      }

      _last = Option(queue.dequeue())
      return _last.get
    } else {
      throw new RuntimeException("No more items!!!");
    }
  }
}

case class Day10()
{
  def getInput(): Array[Char] = Source.fromResource("day10.data").getLines.take(1).toList.head.toCharArray

  def count(iterator : CharIterator) : Int = {
    var l = 0;
    while (iterator.hasNext) {
      iterator.next();
      l += 1
    }
    return l;
  }

  def execute(times : Int) = {
    var input : CharIterator = DefaultCharIterator(getInput)

    for(i <- (1 to times))
      input = LookAndSayIterator(input)

    count(input)
  }

  def time[R](block: => R): R = {
      val t0 = System.currentTimeMillis()
      val result = block    // call-by-name
      val t1 = System.currentTimeMillis()
      println("Elapsed time: " + (t1 - t0) + "ms")
      result
  }

  def part1(): Int = time { execute(40) }

  def part2(): Int = time { execute(50) }

  def execute(): Unit = {
    println();
    println("--- Day 10 ---")
    println(s"Answer to day 10 part 1 is $part1")
    println(s"Answer to day 10 part 2 is $part2")
  }
}
