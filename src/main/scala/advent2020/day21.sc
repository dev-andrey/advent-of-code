val input = scala.io.Source.fromResource(s"advent2020/day21.txt").getLines()

val food = input.map { line =>
  (
    line.takeWhile(_ != '(').split(' '),
    line.dropWhile(_ != '(').drop(9).dropRight(1).split(',').map(_.trim)
  )
}.toList

val ingredients = food.flatMap(_._1).groupMapReduce(identity)(_ => 1)(_ + _)

val guesses = food.flatMap { case (ingredients, allergens) =>
  allergens.map(al => al -> ingredients.toSet)
}.groupMapReduce(_._1)(combo => combo._2)(_ intersect _)

@annotation.tailrec
def guess(facts: Map[String, String], unknown: Map[String, Set[String]]): Map[String, String] =
  if (unknown.isEmpty) facts
  else {
    val newFacts      = facts ++ unknown.filter(_._2.size == 1).view.mapValues(_.head)
    val remainUnknown = unknown.filter(_._2.size != 1).view.mapValues(_ diff newFacts.values.toSet).toMap
    guess(newFacts, remainUnknown)
  }

val known            = guess(Map.empty[String, String], guesses)
val knownIngredients = known.values.toSet

ingredients
  .filterNot { case (ingredient, _) => knownIngredients(ingredient) }
  .values
  .sum

known.toList.sortBy(_._1).map(_._2).mkString(",")
