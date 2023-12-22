val input = scala.io.Source.fromResource(s"advent2023/day21.txt").getLines().toSeq

case class Pos(x: Int, y: Int):
  def around =
    Seq(Pos(x - 1, y), Pos(x, y - 1), Pos(x + 1, y), Pos(x, y + 1))

val garden = {
  for
    y <- input.indices
    x <- input.head.indices
  yield Pos(x, y) -> input(y)(x)
}.toMap

val start = garden.find(_._2 == 'S').map(_._1).get

def walk(steps: Int, start: Pos): Long =
  def isValid(pos: Pos) = garden.get(pos).exists(_ != '#')

  @scala.annotation.tailrec
  def explore(step: Int, current: Set[Pos], seen: Set[Pos], result: Set[Pos]): Long =
    if step == 0 then (result ++ current).size
    else
      val next = current.flatMap(_.around.filter(isValid).filterNot(seen))
      explore(step - 1, next, seen ++ next, if step % 2 == 0 then result ++ current else result)

  explore(steps, Set(start), Set(start), Set())

val part1 = walk(steps = 64, start = start)

val steps =  26501365
val size  = input.length

val width = steps / size - 1

val oddCount  = math.pow(width / 2 * 2 + 1, 2).toLong
val oddPoints = walk(size * 2 + 1, start)

val evenCount  = math.pow((width + 1) / 2 * 2, 2).toLong
val evenPoints = walk(size * 2, start)

val part2 = oddCount * oddPoints + evenCount * evenPoints
  + walk(size - 1, Pos(start.x, size - 1))
  + walk(size - 1, Pos(0, start.y))
  + walk(size - 1, Pos(start.x, 0))
  + walk(size - 1, Pos(size - 1, start.y))
  + (width + 1) * {
    walk(size / 2 - 1, Pos(0, size - 1))
      + walk(size / 2 - 1, Pos(size - 1, size - 1))
      + walk(size / 2 - 1, Pos(0, 0))
      + walk(size / 2 - 1, Pos(size - 1, 0))
  }
  + width * {
    walk(size * 3 / 2 - 1, Pos(0, size - 1))
      + walk(size * 3 / 2 - 1, Pos(size - 1, size - 1))
      + walk(size * 3 / 2 - 1, Pos(0, 0))
      + walk(size * 3 / 2 - 1, Pos(size - 1, 0))
  }
