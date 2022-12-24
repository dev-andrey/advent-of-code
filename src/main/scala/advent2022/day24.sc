val input = scala.io.Source.fromResource(s"advent2022/day24.txt").getLines().toSeq

val (minX, minY, maxX, maxY) = (0, 0, input.head.length - 1, input.length - 1)

val entranceX = input.head.indexOf(".")
val entranceY = 0
val exitX     = input.last.indexOf(".")
val exitY     = maxY

final case class Loc(x: Int, y: Int) { self =>
  def up = Loc(x, y - 1)

  def right = Loc(x + 1, y)

  def down = Loc(x, y + 1)

  def left = Loc(x - 1, y)

  def +(other: Loc) = Loc(x + other.x, y + other.y)

  def around =
    Seq(up, right, down, left, this)
      .filter {
        case Loc(`entranceX`, `entranceY`) => true
        case Loc(`exitX`, `exitY`)         => true
        case Loc(x, y)                     => x > minX && x < maxX && y > minY && y < maxY
      }

  def distanceTo(other: Loc): Int = (self.x - other.x).abs + (self.y - other.y).abs
}

val start = Loc(entranceX, entranceY)
val end   = Loc(exitX, exitY)

final case class Blizzard(loc: Loc, dir: Loc) {
  def move =
    Blizzard(
      loc + dir match {
        case Loc(x, y) if x == minX => Loc(maxX - 1, y)
        case Loc(x, y) if x == maxX => Loc(minX + 1, y)
        case Loc(x, y) if y == minY => Loc(x, maxY - 1)
        case Loc(x, y) if y == maxY => Loc(x, minY + 1)
        case loc                    => loc
      },
      dir
    )

}

val blizzards =
  input.zipWithIndex.foldLeft(Seq.empty[Blizzard]) { case (blizzards, (line, y)) =>
    blizzards ++ line.zipWithIndex.collect {
      case '>' -> x => Blizzard(Loc(x, y), Loc(1, 0))
      case 'v' -> x => Blizzard(Loc(x, y), Loc(0, 1))
      case '<' -> x => Blizzard(Loc(x, y), Loc(-1, 0))
      case '^' -> x => Blizzard(Loc(x, y), Loc(0, -1))
    }
  }

final case class Path(cur: Loc, time: Int)
def blizzardPaths(time: Int, blizzards: Seq[Blizzard], paths: Map[Int, Set[Loc]]): Map[Int, Set[Loc]] = {
  val next     = blizzards.map(_.move)
  val nextLocs = next.map(_.loc).toSet
  if (paths.exists(_._2 == nextLocs)) paths
  else blizzardPaths(time + 1, next, paths + (time -> nextLocs))
}

val blizzardMap = blizzardPaths(1, blizzards, Map(0 -> blizzards.map(_.loc).toSet))

def findShortest(paths: Seq[Path], end: Loc, minLength: Int): Int = {
  val (finished, exploring) = paths.partition(_.cur == end)
  val shortest              = finished.map(_.time).minOption.getOrElse(minLength)
  val next                  = exploring.filter(_.time < shortest)

  if (next.isEmpty) shortest
  else {
    val nextPaths = next.flatMap { case Path(cur, time) =>
      cur.around.map(loc => Path(loc, time + 1)).filterNot { path =>
        blizzardMap(path.time % blizzardMap.size).contains(path.cur)
      }
    }
    findShortest(nextPaths.distinct, end, shortest)
  }
}

val time  = findShortest(Seq(Path(start, 0)), end, Int.MaxValue)
val time2 = findShortest(Seq(Path(end, time)), start, Int.MaxValue)
val time3 = findShortest(Seq(Path(start, time2)), end, Int.MaxValue)
