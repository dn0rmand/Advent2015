package Days

import scala.io.Source

case class Day15()
{
  case class Ingredient(capacity: Int, 
                        durability: Int, 
                        flavor: Int, 
                        texture: Int, 
                        calories: Int)
  {
    def *(quantity: Int) : Ingredient = Ingredient(
      capacity*quantity, 
      durability*quantity, 
      flavor*quantity, 
      texture*quantity, 
      calories*quantity
    )

    def +(b: Ingredient) : Ingredient = Ingredient( 
        capacity + b.capacity,
        durability + b.durability, 
        flavor + b.flavor,
        texture + b.texture,
        calories + b.calories)

    private def check(v : Int) : Int = if (v < 0) 0 else v

    def score() : Int = check(capacity) * check(durability) * check(flavor) * check(texture)
  }

  case class Recipe(ingredients: Array[Ingredient], caloriesLimit: Int = 0)
  {
    def +(ingredient: Ingredient): Recipe = 
      Recipe(ingredients :+ ingredient, caloriesLimit)

    def score() : Int = {
      val value  = ingredients.fold(Ingredient(0,0,0,0,0))((a, b) => a+b)
      if (caloriesLimit == 0 || value.calories == caloriesLimit)
        value.score()
      else
        0
    }    
  }

  object Recipe
  {
    def empty(caloriesLimit : Int = 0): Recipe = Recipe(Array(), caloriesLimit)
  }

  val parser = "(.*): capacity (.*), durability (.*), flavor (.*), texture (.*), calories (.*)".r

  def getInput(): Array[Ingredient] =
    Source.fromResource("day15.data")
          .getLines
          .map({
            case parser(name, capacity, durability, flavor, texture, calories) 
              => Ingredient(capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
            case _ 
              => throw new RuntimeException("Invalid data")
          })
          .toArray

  def calculate(ingredients: Array[Ingredient], recipe: Recipe, remainingQuantity: Int): Int = {
    if (ingredients.isEmpty) 
    {
      if (remainingQuantity == 0) recipe.score() else 0
    } 
    else if (ingredients.size == 1) // last one ?
    { 
      calculate(ingredients.drop(1), recipe + ingredients(0)*remainingQuantity, 0)
    } 
    else if (remainingQuantity <= 0) 
    {
      0
    } 
    else 
    {
      val scores = 
      for(quantity <- (1 to remainingQuantity))
            yield calculate(ingredients.drop(1), recipe+ ingredients(0)*quantity, remainingQuantity-quantity)
      scores.max
    }
  }

  def part1(): Int = 
    calculate(getInput(), Recipe.empty(), 100); 

  def part2(): Int =
    calculate(getInput(), Recipe.empty(500), 100); 

  def execute(): Unit = {
    println();
    println("--- Day 15 ---")
    println(s"Answer to day 15 part 1 is $part1")
    println(s"Answer to day 15 part 2 is $part2")
  }
}
