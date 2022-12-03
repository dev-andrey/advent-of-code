val input = scala.io.Source.fromResource(s"advent2020/day17.txt").getLines()

final case class Coord3D(x: Int, y: Int, z: Int)

val (active, inactive) = input.zipWithIndex
  .foldLeft((Set.empty[Coord3D], Set.empty[Coord3D])) { case ((active, inactive), (row, y)) =>
    (
      active ++ row.zipWithIndex.collect { case '#' -> x => Coord3D(x, y, 0) },
      inactive ++ row.zipWithIndex.collect { case '.' -> x => Coord3D(x, y, 0) }
    )
  }

def neighbors(coord: Coord3D): Set[Coord3D] = {
  for {
    dx <- -1 to 1
    dy <- -1 to 1
    dz <- -1 to 1
  } yield Coord3D(coord.x + dx, coord.y + dy, coord.z + dz)
}.filter(_ != coord).toSet

def expand(everything: Set[Coord3D]): Set[Coord3D] = {
  for {
    z <- everything.map(_.z).min - 1 to everything.map(_.z).max + 1
    y <- everything.map(_.y).min - 1 to everything.map(_.y).max + 1
    x <- everything.map(_.x).min - 1 to everything.map(_.x).max + 1
  } yield Coord3D(x, y, z)
}.toSet diff everything

@annotation.tailrec
def cycle(remain: Int, active: Set[Coord3D], inactive: Set[Coord3D]): Set[Coord3D] =
  if (remain == 0) active
  else {
    val (remainActive, deactivated) = active.foldLeft((Set.empty[Coord3D], Set.empty[Coord3D])) {
      case ((stillActive, deactivated), coord) =>
        if ((2 to 3) contains (active intersect neighbors(coord)).size) (stillActive + coord, deactivated)
        else (stillActive, deactivated + coord)
    }
    val (activated, remainInactive) = inactive.foldLeft((Set.empty[Coord3D], Set.empty[Coord3D])) {
      case ((activated, remainInactive), coord) =>
        if ((active intersect neighbors(coord)).size == 3) (activated + coord, remainInactive)
        else (activated, remainInactive + coord)
    }

    val expanded: Set[Coord3D] = expand(remainActive ++ deactivated ++ activated ++ remainInactive)

    cycle(remain - 1, remainActive ++ activated, remainInactive ++ deactivated ++ expanded)
  }

cycle(6, active, inactive ++ expand(active ++ inactive)).size
