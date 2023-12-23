val input = scala.io.Source.fromResource(s"advent2023/day23.txt").getLines().toSeq

case class Pos(x: Int, y: Int):
  def isValid             = x >= 0 && x <= input.head.length - 1 && y >= 0 && y <= input.length - 1
  def moveBy(offset: Pos) = this.copy(x = x + offset.x, y = y + offset.y)

val start = Pos(input.head.indexOf('.'), 0)
val end   = Pos(input.last.indexOf('.'), input.length - 1)

val left  = Pos(-1, 0)
val up    = Pos(0, -1)
val right = Pos(1, 0)
val down  = Pos(0, 1)

val crossroads = for
  y <- input.indices
  x <- input.head.indices
  if input(y)(x) != '#'
  if Seq(left, up, right, down).map(_.moveBy(Pos(x, y))).filter(_.isValid).count(p => input(p.y)(p.x) != '#') >= 3
yield Pos(x, y)

val poi = Seq(start, end) ++ crossroads

@scala.annotation.tailrec
def walk(explore: Seq[Pos], getNext: Pos => Seq[Pos], graph: Map[Pos, Map[Pos, Int]]): Map[Pos, Map[Pos, Int]] =
  explore match
    case Seq()            => graph
    case starting +: tail =>
      @scala.annotation.tailrec
      def distance(walking: Seq[(Pos, Int)], seen: Set[Pos], graph: Map[Pos, Map[Pos, Int]]): Map[Pos, Map[Pos, Int]] =
        walking match
          case Seq()                                                => graph
          case (pos, dist) +: tail if dist > 0 && poi.contains(pos) =>
            distance(
              walking.tail,
              seen,
              graph.updated(starting, graph.getOrElse(starting, Map.empty) + (pos -> dist))
            )
          case (pos, dist) +: tail                                  =>
            val nextWalk = getNext(pos)
              .map(_.moveBy(pos))
              .filter(_.isValid)
              .filterNot(seen)
              .filter(pos => input(pos.y)(pos.x) != '#')

            distance(nextWalk.map(pos => (pos, dist + 1)) ++ walking.tail, seen ++ nextWalk, graph)

      val updatedGraph = distance(Seq((starting, 0)), Set(starting), graph)
      walk(explore.tail, getNext, updatedGraph)

def longestPath(pos: Pos, seen: Set[Pos])(graph: Map[Pos, Map[Pos, Int]]): Int =
  if pos == end then 0
  else
    graph(pos)
      .filterNot { case (next, _) => seen(next) }
      .map { case (next, distance) =>
        distance + longestPath(next, seen + pos)(graph)
      }
      .maxOption
      .getOrElse(-1)

/* PART 1 */
val withSlopes = (pos: Pos) =>
  input(pos.y)(pos.x) match
    case '>' => Seq(right)
    case 'v' => Seq(down)
    case '<' => Seq(left)
    case '.' => Seq(left, up, right, down)
    case '#' => Seq()

val part1 = longestPath(start, Set.empty)(walk(poi, withSlopes, Map(end -> Map.empty)))

/* PART 2 */
val withoutSlopes = (pos: Pos) =>
  input(pos.y)(pos.x) match
    case '#' => Seq()
    case _   => Seq(left, up, right, down)

val part2 = longestPath(start, Set.empty)(walk(poi, withoutSlopes, Map(end -> Map.empty)))
