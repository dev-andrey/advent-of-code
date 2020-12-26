val input = scala.io.Source.fromResource(s"advent2020/day17.txt").getLines()

final case class Coord4D(x: Int, y: Int, z: Int, w: Int)

val (active, inactive) = input
  .zipWithIndex
  .foldLeft((Set.empty[Coord4D], Set.empty[Coord4D])) { case ((active, inactive), (row, y)) =>
    (
      active ++ row.zipWithIndex.collect { case '#' -> x => Coord4D(x, y, 0, 0) },
      inactive ++ row.zipWithIndex.collect { case '.' -> x => Coord4D(x, y, 0, 0) }
    )
  }

def neighbors(coord: Coord4D): Set[Coord4D] = {
  for {
    dx <- -1 to 1
    dy <- -1 to 1
    dz <- -1 to 1
    dw <- -1 to 1
  } yield Coord4D(coord.x + dx, coord.y + dy, coord.z + dz, coord.w + dw)
}.filter(_ != coord).toSet

def neighbors2(coord: Vector[Int]): Set[Vector[Int]] =
  coord
    .indices
    .foldLeft(Set(coord)) { case out -> idx =>
      out.flatMap(vec => Vector(-1, 0, 1).map(offset => vec.updated(idx, vec(idx) + offset)))
    }
    .filterNot(_ == coord)

def expand(everything: Set[Coord4D]): Set[Coord4D] = {
  for {
    w <- everything.map(_.w).min - 1 to everything.map(_.w).max + 1
    z <- everything.map(_.z).min - 1 to everything.map(_.z).max + 1
    y <- everything.map(_.y).min - 1 to everything.map(_.y).max + 1
    x <- everything.map(_.x).min - 1 to everything.map(_.x).max + 1
  } yield Coord4D(x, y, z, w)
}.toSet diff everything

def expand2(universe: Set[Vector[Int]]): Set[Vector[Int]] =
  universe.foldLeft((Vector.fill(universe.head.size)(0), Vector.fill(universe.head.size)(0))) {
    case (min, max) -> coord =>
      (
        (min zip coord).map { case (m, c) => m min c },
        (max zip coord).map { case (m, c) => m max c }
      )
  } match {
    case (minCoord, maxCoord) => ???
  }

val expansion = expand2(Set(Vector(0, 0, 0, 0)))
expansion.size
expand(Set(Coord4D(0, 0, 0, 0))).size

@annotation.tailrec
def cycle(remain: Int, active: Set[Coord4D], inactive: Set[Coord4D]): Set[Coord4D] =
  if (remain == 0) active
  else {
    val (remainActive, deactivated) = active.foldLeft((Set.empty[Coord4D], Set.empty[Coord4D])) {
      case ((stillActive, deactivated), coord) =>
        if ((2 to 3) contains (active intersect neighbors(coord)).size) (stillActive + coord, deactivated)
        else (stillActive, deactivated + coord)
    }
    val (activated, remainInactive) = inactive.foldLeft((Set.empty[Coord4D], Set.empty[Coord4D])) {
      case ((activated, remainInactive), coord) =>
        if ((active intersect neighbors(coord)).size == 3) (activated + coord, remainInactive)
        else (activated, remainInactive + coord)
    }

    val expanded = expand(remainActive ++ deactivated ++ activated ++ remainInactive)

    cycle(remain - 1, remainActive ++ activated, remainInactive ++ deactivated ++ expanded)
  }

cycle(0, active, inactive ++ expand(active ++ inactive)).size
