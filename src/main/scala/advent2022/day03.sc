val rucksacks = scala.io.Source.fromResource(s"advent2022/day03.txt").getLines().toList

// part 1
rucksacks
  .map(items => items.splitAt(items.length / 2))
  .map { case (left, right) => (left intersect right).head }
  .map {
    case item if item.isLower => item - 'a' + 1
    case item                 => item - 'A' + 1 + 26
  }
  .sum

// part 2
rucksacks
  .grouped(3)
  .collect { case first :: second :: third :: Nil =>
    first intersect second intersect third
  }
  .map(_.head)
  .map {
    case item if item.isLower => item - 'a' + 1
    case item                 => item - 'A' + 1 + 26
  }
  .sum
