val input  = scala.io.Source.fromResource(s"advent2020/day12.txt").getLines()
val parsed = input.map(cmd => (cmd.take(1).head, cmd.drop(1).toInt)).toList

final case class Loc(x: Int, y: Int)

@annotation.tailrec
def move(dir: Int, loc: Loc, cmds: List[(Char, Int)]): Int =
  if (cmds.isEmpty) loc.x.abs + loc.y.abs
  else
    cmds.head match {
      case ('N', dy) => move(dir, loc.copy(y = loc.y + dy), cmds.tail)
      case ('E', dx) => move(dir, loc.copy(x = loc.x + dx), cmds.tail)
      case ('S', dy) => move(dir, loc.copy(y = loc.y - dy), cmds.tail)
      case ('W', dx) => move(dir, loc.copy(x = loc.x - dx), cmds.tail)
      case ('L', deg) =>
        (dir - deg) % 360 match {
          case degrees if degrees < 0 => move(360 + degrees, loc, cmds.tail)
          case degrees                => move(degrees, loc, cmds.tail)
        }
      case ('R', deg) => move((dir + deg) % 360, loc, cmds.tail)
      case ('F', units) =>
        dir match {
          case 0   => move(dir, loc.copy(y = loc.y + units), cmds.tail)
          case 90  => move(dir, loc.copy(x = loc.x + units), cmds.tail)
          case 180 => move(dir, loc.copy(y = loc.y - units), cmds.tail)
          case 270 => move(dir, loc.copy(x = loc.x - units), cmds.tail)
        }
    }

move(90, Loc(0, 0), parsed)

@annotation.tailrec
def move(ship: Loc, wp: Loc, cmds: List[(Char, Int)]): Int =
  if (cmds.isEmpty) ship.x.abs + ship.y.abs
  else
    cmds.head match {
      case ('N', dy) => move(ship, wp.copy(y = wp.y + dy), cmds.tail)
      case ('E', dx) => move(ship, wp.copy(x = wp.x + dx), cmds.tail)
      case ('S', dy) => move(ship, wp.copy(y = wp.y - dy), cmds.tail)
      case ('W', dx) => move(ship, wp.copy(x = wp.x - dx), cmds.tail)
      case ('L', degrees) =>
        degrees match {
          case 0 | 360 => move(ship, wp, cmds.tail)
          case 90      => move(ship, wp.copy(x = -wp.y, y = wp.x), cmds.tail)
          case 180     => move(ship, wp.copy(x = -wp.x, y = -wp.y), cmds.tail)
          case 270     => move(ship, wp.copy(x = wp.y, y = -wp.x), cmds.tail)
        }
      case ('R', degrees) =>
        degrees match {
          case 0 | 360 => move(ship, wp, cmds.tail)
          case 90      => move(ship, wp.copy(x = wp.y, y = -wp.x), cmds.tail)
          case 180     => move(ship, wp.copy(x = -wp.x, y = -wp.y), cmds.tail)
          case 270     => move(ship, wp.copy(x = -wp.y, y = wp.x), cmds.tail)
        }
      case ('F', units) => move(ship.copy(x = ship.x + units * wp.x, y = ship.y + units * wp.y), wp, cmds.tail)
    }

move(Loc(0, 0), Loc(10, 1), parsed)
