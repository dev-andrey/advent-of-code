val input = scala.io.Source.fromResource(s"advent2022/day08.txt").getLines().toSeq

final case class Loc(x: Int, y: Int)

val maxX = input.head.length
val (maxY, forest) = input
  .foldLeft(0, Map.empty[Loc, Int]) { case (row, map) -> line =>
    (row + 1, map ++ line.zipWithIndex.map { case (height, x) => (Loc(x, row), height.asDigit) }.toMap)
  }

// part 1
def isVisible(tree: (Loc, Int)): Boolean =
  (0 until tree._1.x).forall(x => forest(Loc(x, tree._1.y)) < tree._2) ||
    (0 until tree._1.y).forall(y => forest(Loc(tree._1.x, y)) < tree._2) ||
    (tree._1.x + 1 until maxX).forall(x => forest(Loc(x, tree._1.y)) < tree._2) ||
    (tree._1.y + 1 until maxY).forall(y => forest(Loc(tree._1.x, y)) < tree._2)

forest.count(isVisible)

// part 2
def scenicScore(tree: (Loc, Int)): Int = {
  val left   = (tree._1.x - 1 until 0 by -1).takeWhile(x => forest(Loc(x, tree._1.y)) < tree._2).length + 1
  val top    = (tree._1.y - 1 until 0 by -1).takeWhile(y => forest(Loc(tree._1.x, y)) < tree._2).length + 1
  val right  = (tree._1.x + 1 until maxX - 1).takeWhile(x => forest(Loc(x, tree._1.y)) < tree._2).length + 1
  val bottom = (tree._1.y + 1 until maxY - 1).takeWhile(y => forest(Loc(tree._1.x, y)) < tree._2).length + 1

  left * top * right * bottom
}

forest.filter(isVisible).map(scenicScore).max
