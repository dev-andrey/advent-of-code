import scala.collection.mutable

val heatMap = scala.io.Source.fromResource(s"advent2023/day17.txt").getLines().toSeq
val maxX    = heatMap.head.length - 1
val maxY    = heatMap.length - 1

case class Pos(x: Int, y: Int):
  def isValid       = x >= 0 && x <= maxX && y >= 0 && y <= maxY
  def +(other: Pos) = this.copy(x = x + other.x, y = y + other.y)

val start = Pos(0, 0)
val end   = Pos(maxX, maxY)

val Up    = Pos(0, -1)
val Down  = Pos(0, 1)
val Left  = Pos(-1, 0)
val Right = Pos(1, 0)

val directions = Set(Up, Down, Left, Right)
val opposite   = Map(Up -> Down, Left -> Right, Right -> Left, Down -> Up)

case class Crucible(pos: Pos, dir: Pos, steps: Int, loss: Int)

def normalLoss(losses: mutable.PriorityQueue[Crucible], visited: Map[(Pos, Pos, Int), Int]): Int =
  if losses.isEmpty then -1
  else
    val crucible = losses.dequeue()
    if crucible.pos == end then crucible.loss
    else if visited.get((crucible.pos, crucible.dir, crucible.steps)).exists(_ <= crucible.loss) then
      normalLoss(losses, visited)
    else
      val nextCrucibles = directions
        .filter(direction => (crucible.pos + direction).isValid)
        .filterNot(direction => direction == opposite(crucible.dir))
        .filterNot(direction => crucible.dir == direction && crucible.steps == 3)
        .map(direction =>
          val nextPos  = crucible.pos + direction
          val nextLoss = crucible.loss + heatMap(nextPos.y)(nextPos.x).asDigit
          Crucible(
            nextPos,
            direction,
            if crucible.dir == direction then crucible.steps + 1 else 1,
            nextLoss
          )
        )
        .filterNot(next => visited.get((next.pos, next.dir, next.steps)).exists(_ <= next.loss))

      normalLoss(losses ++ nextCrucibles, visited + ((crucible.pos, crucible.dir, crucible.steps) -> crucible.loss))

val part1 = normalLoss(
  mutable.PriorityQueue.from(Seq(Crucible(start, Right, 0, 0))) { (x: Crucible, y: Crucible) =>
    y.loss compareTo x.loss
  },
  Map.empty
)

def ultraLoss(losses: mutable.PriorityQueue[Crucible], visited: Map[(Pos, Pos, Int), Int]): Int =
  if losses.isEmpty then -1
  else
    val crucible = losses.dequeue()
    if crucible.pos == end && crucible.steps >= 4 then crucible.loss
    else if crucible.pos == end then ultraLoss(losses, visited)
    else if visited.get((crucible.pos, crucible.dir, crucible.steps)).exists(_ <= crucible.loss) then
      ultraLoss(losses, visited)
    else
      val nextCrucibles = directions
        .filter(direction => (crucible.pos + direction).isValid)
        .filterNot(direction => direction == opposite(crucible.dir))
        .filterNot(direction => crucible.dir == direction && crucible.steps == 10)
        .map(direction =>
          val nextPos  = crucible.pos + direction
          val nextLoss = crucible.loss + heatMap(nextPos.y)(nextPos.x).asDigit
          Crucible(
            nextPos,
            direction,
            if crucible.dir == direction then crucible.steps + 1 else 1,
            nextLoss
          )
        )
        .filterNot(next => crucible.steps < 4 && next.dir != crucible.dir)
        .filterNot(next => visited.get((next.pos, next.dir, next.steps)).exists(_ <= next.loss))

      ultraLoss(losses ++ nextCrucibles, visited + ((crucible.pos, crucible.dir, crucible.steps) -> crucible.loss))

val part2 = ultraLoss(
  mutable.PriorityQueue.from(Seq(Crucible(start, Right, 0, 0), Crucible(start, Down, 0, 0))) {
    (x: Crucible, y: Crucible) =>
      y.loss compareTo x.loss
  },
  Map.empty
)
