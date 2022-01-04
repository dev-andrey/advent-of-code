import scala.annotation.tailrec

val raw = scala.io.Source.fromResource(s"advent2021/day25.txt").getLines()

final case class Loc(x: Int, y: Int)

val (maxRow, maxCol, eastFacing, southFacing) = raw.foldLeft((0, 0, Set.empty[Loc], Set.empty[Loc])) {
  case (row, _, east, south) -> line =>
    (
      row + 1,
      line.length,
      east ++ line.zipWithIndex.collect { case ('>', col) =>
        Loc(col, row)
      },
      south ++ line.zipWithIndex.collect { case ('v', col) =>
        Loc(col, row)
      }
    )
}
val minX = 0
val minY = 0
val maxX = maxCol - 1
val maxY = maxRow - 1

def draw(east: Set[Loc], south: Set[Loc]) =
  for {
    y <- 0 to maxY
    x <- 0 to maxX
  } yield {

    if (east.contains(Loc(x, y))) print('>')
    else if (south.contains(Loc(x, y))) print('v')
    else print('.')
    if (x == maxX) println()
  }

@tailrec
def move(step: Int, east: Set[Loc], south: Set[Loc]): Int = {
  val newEast = east.map { loc =>
    val newLoc = loc.copy(x = (loc.x + 1) % maxCol)
    if (east.contains(newLoc) || south.contains(newLoc)) loc
    else newLoc
  }
  val newSouth =
    south.map { loc =>
      val newLoc = loc.copy(y = (loc.y + 1) % maxRow)
      if (newEast.contains(newLoc) || south.contains(newLoc)) loc
      else newLoc
    }
  if (newEast == east && newSouth == south) step + 1
  else move(step + 1, newEast, newSouth)
}

move(0, eastFacing, southFacing)
