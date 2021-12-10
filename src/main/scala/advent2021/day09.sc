val raw = scala.io.Source.fromResource(s"advent2021/day09.txt").getLines()

val (maxRow, maxCol, input) = raw.foldLeft((0, 0, Map.empty[(Int, Int), Int])) { case (row, _, grid) -> line =>
  (row + 1, line.length, grid ++ line.zipWithIndex.map(_.swap).map { case (x, ch) => (x, row) -> ch.asDigit })
}
val (maxX, maxY) = (maxCol - 1, maxRow - 1)

// Part 1

final case class Loc(loc: (Int, Int)) { self =>
  def left: Set[Loc] =
    self.loc match {
      case (0, _) => Set()
      case (x, y) => Set(Loc(x - 1, y))
    }

  def top: Set[Loc] =
    self.loc match {
      case (_, 0) => Set()
      case (x, y) => Set(Loc(x, y - 1))
    }

  def right: Set[Loc] =
    self.loc match {
      case (`maxX`, _) => Set()
      case (x, y)      => Set(Loc(x + 1, y))
    }

  def bottom: Set[Loc] =
    self.loc match {
      case (_, `maxY`) => Set()
      case (x, y)      => Set(Loc(x, y + 1))
    }

  def adjacent: Set[Loc] =
    left ++ top ++ right ++ bottom

}

val lowestPoints = for {
  x <- 0 to maxX
  y <- 0 to maxY
  if input((x, y)) < Loc((x, y)).adjacent.map(l => input(l.loc)).min
} yield Loc(x, y)

lowestPoints
  .map(loc => input(loc.loc) + 1)
  .sum

// part 2
@scala.annotation.tailrec
def fill(remaining: Seq[Loc], edges: Map[Loc, Set[Loc]], basins: Map[Loc, Set[Loc]]): Map[Loc, Set[Loc]] =
  if (remaining.isEmpty) basins
  else {
    val newEdges = edges.map { case (loc, edge) =>
      loc -> edge.flatMap(edge => edge.adjacent diff basins(loc)).filter(loc => input(loc.loc) < 9)
    }
    fill(
      remaining.filter(loc => newEdges(loc).nonEmpty),
      newEdges,
      basins.map { case (loc, locs) => loc -> (locs ++ edges(loc)) }
    )
  }

fill(
  remaining = lowestPoints.toList,
  edges = lowestPoints.map(loc => loc -> Set(loc)).toMap,
  basins = lowestPoints.map(loc => loc -> Set.empty[Loc]).toMap
)
  .values
  .map(_.size)
  .toVector
  .sorted
  .takeRight(3)
  .product
