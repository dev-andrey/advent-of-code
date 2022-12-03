val input = scala.io.Source.fromResource(s"advent2020/day03.txt").getLines().toList

input.foldLeft((0, 0)) { case (result, position) -> row =>
  (result + (if (row(position) == '#') 1 else 0), (position + 3) % row.length)
}

def countTrees(rightBy: Int, downBy: Int): Int =
  input.zipWithIndex
    .filter { case (_, idx) =>
      downBy == 1 || (idx % 2 == 0)
    }
    .map(_._1)
    .foldLeft((0, 0)) { case (result, position) -> row =>
      (result + (if (row(position) == '#') 1 else 0), (position + rightBy) % row.length)
    }
    ._1

List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2)).map { case (rightBy, downBy) =>
  countTrees(rightBy, downBy)
}.product
