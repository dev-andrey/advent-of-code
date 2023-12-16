val layout = scala.io.Source.fromResource(s"advent2023/day16.txt").getLines().toSeq

enum Direction:
  case Left
  case Up
  case Down
  case Right

import Direction.*

case class Loc(x: Int, y: Int):
  def isValid = x >= 0 && x <= layout.head.length - 1 && y >= 0 && y <= layout.length - 1

case class Beam(direction: Direction, loc: Loc):
  def next: Loc = direction match
    case Left  => loc.copy(x = loc.x - 1)
    case Up    => loc.copy(y = loc.y - 1)
    case Right => loc.copy(x = loc.x + 1)
    case Down  => loc.copy(y = loc.y + 1)

def simulate(beams: Seq[Beam], energized: Set[Beam]): Set[Loc] =
  if beams.isEmpty then energized.map(_.loc)
  else
    val updatedBeams = beams
      .flatMap { beam =>
        val nextLoc @ Loc(x, y) = beam.next
        if nextLoc.isValid then
          (beam.direction, layout(y)(x)) match
            case (Left, '/')  => Seq(beam.copy(direction = Down, loc = nextLoc))
            case (Up, '/')    => Seq(beam.copy(direction = Right, loc = nextLoc))
            case (Down, '/')  => Seq(beam.copy(direction = Left, loc = nextLoc))
            case (Right, '/') => Seq(beam.copy(direction = Up, loc = nextLoc))

            case (Left, '\\')  => Seq(beam.copy(direction = Up, loc = nextLoc))
            case (Up, '\\')    => Seq(beam.copy(direction = Left, loc = nextLoc))
            case (Down, '\\')  => Seq(beam.copy(direction = Right, loc = nextLoc))
            case (Right, '\\') => Seq(beam.copy(direction = Down, loc = nextLoc))

            case (Up | Down, '-') =>
              Seq(
                beam.copy(direction = Left, loc = nextLoc),
                beam.copy(direction = Right, loc = nextLoc)
              )

            case (Left | Right, '|') =>
              Seq(
                beam.copy(direction = Up, loc = nextLoc),
                beam.copy(direction = Down, loc = nextLoc)
              )

            case _ => Seq(beam.copy(loc = nextLoc))
        else Seq()
      }
    simulate(updatedBeams.filterNot(energized), energized ++ updatedBeams)

val starting = Beam(Right, Loc(0, 0))
val part1    = simulate(Seq(starting), Set(starting)).size

val part2 = {
  layout.head.indices.map(x => Beam(Down, Loc(x, 0)))
    ++ layout.head.indices.map(x => Beam(Up, Loc(x, layout.length - 1)))
    ++ layout.indices.map(y => Beam(Right, Loc(0, y)))
    ++ layout.indices.map(y => Beam(Left, Loc(layout.head.length - 1, y)))
}.map(beam => simulate(Seq(beam), Set(beam)))
  .map(_.size)
  .max
