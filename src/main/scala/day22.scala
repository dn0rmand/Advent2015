package Days

import scala.io.Source
import scala.math
import scala.runtime.IntRef

case class Day22()
{
  case class Boss(val hitPoints: Int, val damage: Int) 
  {
    def key : String = s"b($hitPoints)"

    def -(spell: Spell) : Boss = Boss(hitPoints - spell.damage, damage);
  }

  case class Wizard(var mana: Int, val armor: Int, val hitPoints: Int)
  {
    def key : String = s"w($mana:$armor:$hitPoints)"

    def reset : Wizard = Wizard(mana, 0, hitPoints)

    def -(damage: Int) : Wizard = Wizard(mana, armor, hitPoints-damage)
    def -(ennemy: Boss) : Wizard = this - (ennemy.damage - armor)

    def +(spell: Spell) : Wizard =
      Wizard(mana + spell.mana, armor + spell.armor, if (hitPoints <= 0) 0 else hitPoints + spell.heal)
  }

  class Spell(
    val id: Int,
    val cost: Int, 
    val duration: Int, 
    val damage: Int = 0, 
    val heal: Int = 0, 
    val armor: Int = 0,
    val mana: Int = 0)
  {
    def key : String = s"s($id:$duration)"

    def +(player: Wizard) : Wizard = {
      val w = player + this
      Wizard(w.mana - this.cost, w.armor, w.hitPoints)
    }

    def next: Spell = new Spell(id, cost, duration-1, damage, heal, armor, mana)
  }

  case class MagicMissile() extends Spell(id=1, cost=53, duration=1, damage=4)
  case class Drain() extends Spell(id=2, cost=73, duration=1, damage=2, heal=2)
  case class Shield() extends Spell(id=3, cost=113, duration=6, armor=7)
  case class Poison() extends Spell(id=4, cost=173, duration=6, damage=3)
  case class Recharge() extends Spell(id=5, cost=229, duration=5, mana=101)
  
  val allSpells = Array( MagicMissile(), Drain(), Shield(), Poison(), Recharge() )

  case class Players(val wizard: Wizard, val boss: Boss, val effects: Array[Spell] = Array(), val totalCost:Int = 0)
  {    
    def key: String =
      effects.sortWith((a, b) => a.id < b.id).foldLeft(wizard.key+boss.key)((a, b) => a+b.key);

    def applyEffects: Players =
    {
      var w = wizard.reset
      var b = boss

      val newEffects = effects.map(spell => {
        w = w + spell
        b = b - spell
        spell.next
      });

      Players(w,  b, newEffects.filter(e => e.duration > 0), totalCost)
    }

    def canAdd(spell: Spell) : Boolean = !effects.exists( _.id == spell.id ) && spell.cost <= wizard.mana

    def +(spell: Spell) : Players =
    {
      if (! canAdd(spell))
        throw new RuntimeException("Cannot add this spell");

      if(spell.duration > 1) {
        Players(spell + wizard, boss - spell, effects :+ spell.next, totalCost + spell.cost)
      }
      else
        Players(spell + wizard, boss - spell, effects, totalCost + spell.cost)
    }

    def WizardTurn(extraHitPoints: Int) : Array[Players] = {
      val players = Players(wizard-extraHitPoints, boss, effects, totalCost).applyEffects;
      allSpells.filter(spell => players.canAdd(spell))
               .map(spell => players + spell)
    }

    def BossTurn : Players = {
      val players = applyEffects;
      Players(players.wizard - players.boss, players.boss, players.effects, players.totalCost)
    }
  }

  def getInput: Boss = {
    var hitPoints = 0
    var damage = 0

    val hitPointParser = "Hit Points: (.*)".r
    val damageParser = "Damage: (.*)".r

    Source.fromResource("day22.data")
          .getLines
          .foreach {
            case hitPointParser(value) => hitPoints = value.toInt
            case damageParser(value) => damage = value.toInt
          }

    new Boss(hitPoints, damage)
  }

  def run(player: Wizard, boss: Boss, hard: Boolean): Int = 
  {
    var players = Array(Players(player, boss))
    var minCost = Int.MaxValue;
    val extraHitPoints = if (hard) 1 else 0

    while (players.length > 0)
    {
      // Wizard plays

      players = players.map(_.WizardTurn(extraHitPoints)).flatten
      minCost = players.foldLeft(minCost)((a, p) => {
        if (p.boss.hitPoints <= 0 && p.totalCost < a && p.wizard.hitPoints > 0) 
          p.totalCost 
        else 
          a
      })
      players = players.filter(p => p.totalCost < minCost && p.boss.hitPoints > 0 && p.wizard.hitPoints > 0)

      // Boss Plays

      players = players.map(_.BossTurn).filter(_.wizard.hitPoints > 0);

      val groups = players.groupBy(p => p.key)
      players    = groups.values.map(ps => ps.minBy(p => p.totalCost)).toArray
    }

    minCost
  }

  def part1: Int = {
    val boss = getInput
    val wizard = Wizard(mana=500, armor=0, hitPoints=50)
    run(wizard, boss, false)
  }

  def part2: Int = {
    val boss = getInput
    val wizard = Wizard(mana=500, armor=0, hitPoints=50)
    run(wizard, boss, true)
  }

  def execute(): Unit = {
    println();
    println("--- Day 22 ---")
    println(s"Answer to day 22 part 1 is $part1")
    println(s"Answer to day 22 part 2 is $part2")
  }
}
