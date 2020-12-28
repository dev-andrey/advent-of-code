val input = scala.io.Source.fromResource(s"advent2015/day03.txt").toList

final case class Loc(x: Int, y: Int) { self =>
  def move(dir: Char): Loc = dir match {
    case '^' => self.copy(y = y - 1)
    case '>' => self.copy(x = x + 1)
    case 'v' => self.copy(y = y + 1)
    case '<' => self.copy(x = x - 1)
  }
}

// part 1
@annotation.tailrec
def visitHouses(turn: Int, santa: Loc, route: List[Char], visited: Set[Loc]): Set[Loc] =
  if (route.isEmpty) visited
  else visitHouses(turn + 1, santa.move(route.head), route.tail, visited + santa.move(route.head))

visitHouses(0, Loc(0, 0), input, Set(Loc(0, 0))).size

// part 2
@annotation.tailrec
def visitHouses(turn: Int, santa: Loc, robot: Loc, route: List[Char], visited: Set[Loc]): Set[Loc] =
  if (route.isEmpty) visited
  else if (turn % 2 == 0)
    visitHouses(turn + 1, santa.move(route.head), robot, route.tail, visited + santa.move(route.head))
  else visitHouses(turn + 1, santa, robot.move(route.head), route.tail, visited + robot.move(route.head))

visitHouses(0, Loc(0, 0), Loc(0, 0), input, Set(Loc(0, 0))).size
