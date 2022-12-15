val input = scala.io.Source.fromResource(s"advent2022/day14.txt").getLines().toSeq

final case class Loc(x: Int, y: Int) { self =>
  def under: Loc     = self.copy(y = self.y + 1)
  def downLeft: Loc  = self.copy(x = self.x - 1, y = self.y + 1)
  def downRight: Loc = self.copy(x = self.x + 1, y = self.y + 1)
}

val cave = input.flatMap { line =>
  line
    .split(" -> ")
    .map(_.split(","))
    .map(loc => Loc(loc.head.toInt, loc.last.toInt))
    .sliding(2)
    .flatMap {
      case locs if locs.head.x == locs.last.x =>
        ((locs.head.y min locs.last.y) to (locs.head.y max locs.last.y)).map(y => Loc(locs.head.x, y))
      case locs =>
        ((locs.head.x min locs.last.x) to (locs.head.x max locs.last.x)).map(x => Loc(x, locs.head.y))
    }
}.toSet

val maxY = cave.maxBy(_.y).y
val start = Loc(500, 0)

def infiniteFall(count: Int, blocked: Set[Loc]): Int = {
  def findLanding(sand: Loc): Option[Loc] =
    if (sand.y > maxY) None // abyss
    else if (blocked.contains(sand.under))
      if (blocked.contains(sand.downLeft))
        if (blocked.contains(sand.downRight)) Some(sand)
        else findLanding(sand.downRight)
      else findLanding(sand.downLeft)
    else findLanding(sand.under)

  findLanding(start) match {
    case Some(loc) => infiniteFall(count + 1, blocked + loc)
    case None      => count
  }
}

// part 1
infiniteFall(0, cave)

def fallWithFloor(count: Int, blocked: Set[Loc]): Int = {
  def findLanding(sand: Loc): Option[Loc] =
    if (blocked.contains(sand)) None // no place to go
    else if (blocked.contains(sand.under) || sand.under.y == maxY + 2)
      if (blocked.contains(sand.downLeft) || sand.downLeft.y == maxY + 2)
        if (blocked.contains(sand.downRight) || sand.downRight.y == maxY + 2) Some(sand)
        else findLanding(sand.downRight)
      else findLanding(sand.downLeft)
    else findLanding(sand.under)

  findLanding(start) match {
    case Some(loc) => fallWithFloor(count + 1, blocked + loc)
    case None      => count
  }
}

// part 2
fallWithFloor(0, cave)
