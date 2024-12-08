package Days

import scala.io.Source
import scala.math

case class Day21()
{
  class Item(val cost: Int, val damage: Int, val armor: Int)

  case class Weapon(cost1: Int, damage1: Int) extends Item(cost1, damage1, 0)
  case class Armor(cost1: Int, armor1: Int) extends Item(cost1, 0, armor1)
  case class Ring(cost1: Int, damage1: Int, armor1: Int) extends Item(cost1, damage1, armor1)

  case class Player(hitPoints: Int, damage: Int, armor: Int)
  {
    def +(item: Item) : Player = 
      Player(hitPoints, damage+item.damage, armor+item.armor)

    def -(ennemy: Player) : Boolean = {
      val force1 = math.max(1, damage - ennemy.armor)
      val force2 = math.max(1, ennemy.damage - armor);
      val turns = ennemy.hitPoints / force1
      return (turns * force2 < hitPoints);
    }
  }

  def Weapons(): List[Weapon] =
    List(
      Weapon( 8, 4),
      Weapon(10, 5),
      Weapon(25, 6),
      Weapon(40, 7),
      Weapon(74, 8),
    )

  def Armors() : List[Armor] =
    List(
      Armor( 13, 1),
      Armor( 31, 2),
      Armor( 53, 3),
      Armor( 75, 4),
      Armor(102, 5),
    )

  def Rings() : List[Ring] = 
    List(
      Ring( 25, 1, 0),
      Ring( 50, 2, 0),
      Ring(100, 3, 0),
      Ring( 20, 0, 1),
      Ring( 40, 0, 2),
      Ring( 80, 0, 3)
    )

  def getInput(): Player = {
    var hitPoints = 0
    var damage = 0
    var armor = 0

    val hitPointParser = "Hit Points: (.*)".r
    val damageParser = "Damage: (.*)".r
    val armorParser = "Armor: (.*)".r

    Source.fromResource("day21.data")
          .getLines
          .foreach {
            case hitPointParser(value) => hitPoints = value.toInt
            case damageParser(value) => damage = value.toInt
            case armorParser(value) => armor = value.toInt
          }

    Player(hitPoints, damage, armor)
  }

  def part1(): Int = {

    def pickRing2(boss: Player, player: Player, cost: Int, rings: List[Ring]) : Int =
    {
      if (player - boss)
        return cost

      val costs = for(ring <- rings) yield
                    if (player+ring - boss) cost + ring.cost else Int.MaxValue
      costs.min
    }

    def pickRing1(boss: Player, player: Player, cost: Int, rings: List[Ring]) : Int =
    {
      if (player - boss)
        return cost

      val costs = for(ring <- rings) 
                    yield pickRing2(boss, player + ring, cost + ring.cost, rings.filterNot(_ == ring))
      costs.min
    }

    def pickArmor(boss: Player, player: Player, cost: Int) : Int =
    {
      if (player - boss)
        return cost

      val costs = for (armor <- Armors()) yield pickRing1(boss, player + armor, cost + armor.cost, Rings())
      costs.min
    }

    val boss = getInput()
    val player = Player(100, 0, 0)
    
    val costs = for(weapon <- Weapons)
      yield pickArmor(boss, player + weapon, weapon.cost)

    costs.min
  }

  def part2(): Int =
  {
    def pickRing2(boss: Player, player: Player, cost: Int, rings: List[Ring]) : Int =
    {
      if (player - boss)
        return 0

      val costs = for(ring <- rings) yield
                    if (player+ring - boss) 0 else cost + ring.cost
      costs.max
    }

    def pickRing1(boss: Player, player: Player, cost: Int, rings: List[Ring]) : Int =
    {
      if (player - boss)
        return 0

      val costs = for(ring <- rings) 
                    yield pickRing2(boss, player + ring, cost + ring.cost, rings.filterNot(_ == ring))
      costs.max
    }

    def pickArmor(boss: Player, player: Player, cost: Int) : Int =
    {
      if (player - boss)
        return 0

      val costs = for (armor <- Armors()) yield pickRing1(boss, player + armor, cost + armor.cost, Rings())
      costs.max
    }

    val boss = getInput()
    val player = Player(100, 0, 0)
    
    val costs = for(weapon <- Weapons)
      yield pickArmor(boss, player + weapon, weapon.cost)

    costs.max
  }

  def execute(): Unit = {
    println();
    println("--- Day 21 ---")
    println(s"Answer to day 21 part 1 is $part1")
    println(s"Answer to day 21 part 2 is $part2")
  }
}
