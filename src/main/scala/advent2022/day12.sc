val map  = scala.io.Source.fromResource(s"advent2022/day12.txt").getLines().map(_.toCharArray).toArray
val maxX = map.head.length
val maxY = map.length

final case class Loc(x: Int, y: Int) { self =>
  private def elevation: Char = map(y)(x) match {
    case 'E'   => 'z'
    case 'S'   => 'a'
    case other => other
  }

  def nearby: Set[Loc] =
    Set(self.copy(x = x - 1), self.copy(y = y - 1), self.copy(x = x + 1), self.copy(y = y + 1))
      .filter(loc => loc.x >= 0 && loc.y >= 0 && loc.x < maxX && loc.y < maxY)
      .filter(loc => loc.elevation - self.elevation <= 1)
}

def search(locs: Vector[(Loc, Int)], visited: Set[Loc]): Int =
  locs.headOption match {
    case None =>
      Int.MaxValue

    case Some((loc, steps)) if map(loc.y)(loc.x) == 'E' =>
      steps

    case Some((loc, _)) if visited.contains(loc) =>
      search(locs.tail, visited)

    case Some((loc, steps)) =>
      search(locs.tail ++ loc.nearby.filterNot(visited).map(loc => (loc, steps + 1)), visited + loc)
  }

def findShortest(starting: Char) =
  for {
    y <- 0 until maxY
    x <- 0 until maxX
    if map(y)(x) == starting
  } yield search(Vector((Loc(x, y), 0)), Set())

// part 1
findShortest('S').min

// part 2
findShortest('a').min
