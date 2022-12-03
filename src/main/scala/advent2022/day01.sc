val calories = scala.io.Source.fromResource(s"advent2022/day01.txt").getLines().toList

val elves = calories.foldRight(List(List.empty[Int])) {
  case food -> elves if food.isEmpty => List.empty :: elves
  case food -> elves                 => (food.toInt :: elves.head) :: elves.tail
}

elves.map(_.sum).max

elves.map(_.sum).sorted.takeRight(3).sum
