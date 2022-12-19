val input = scala.io.Source.fromResource(s"advent2022/day17.txt").toSeq

val minX = 0
val maxX = 6

final case class Edge(x: Long, y: Long)
final case class Shape(locs: Set[Edge]) { self =>
  def leftmost: Edge  = locs.minBy(_.x)
  def rightmost: Edge = locs.maxBy(_.x)
  def down: Shape     = Shape(locs.map(loc => Edge(loc.x, loc.y - 1)))
}

object Shape {
  def make(shape: String, x: Long, y: Long): Shape = shape match {
    case "bar"    => Shape((x to x + 3).map(Edge(_, y)).toSet)
    case "pipe"   => Shape((y to y + 3).map(Edge(x, _)).toSet)
    case "square" => Shape(Set(Edge(x, y), Edge(x + 1, y), Edge(x, y + 1), Edge(x + 1, y + 1)))
    case "corner" => Shape(Set(Edge(x, y), Edge(x + 1, y), Edge(x + 2, y), Edge(x + 2, y + 1), Edge(x + 2, y + 2)))
    case "cross" =>
      Shape(Set(Edge(x, y + 1), Edge(x + 1, y + 1), Edge(x + 1, y), Edge(x + 1, y + 2), Edge(x + 2, y + 1)))
  }

}

def falling(shape: Shape, edges: Set[Edge], wind: Vector[Char]): (Vector[Char], Shape) = {
  val maybeShifted = wind.head match {
    case '>' if shape.rightmost.x == maxX => shape
    case '<' if shape.leftmost.x == minX  => shape

    case '>' => shape.copy(shape.locs.map(edge => edge.copy(x = edge.x + 1)))
    case '<' => shape.copy(shape.locs.map(edge => edge.copy(x = edge.x - 1)))
  }
  val shifted = if (maybeShifted.locs.intersect(edges).nonEmpty) shape else maybeShifted
  val downed  = shifted.down
  if (downed.locs.intersect(edges).nonEmpty) (wind.tail :+ wind.head, shifted)
  else falling(downed, edges, wind.tail :+ wind.head)
}

@annotation.tailrec
def simulate(
  cnt: Long,
  edges: Set[Edge],
  shapes: Vector[String],
  wind: Vector[Char],
  floor: Long,
  heights: Vector[Long]
): Vector[Long] =
  if (cnt == 0) heights :+ edges.maxBy(_.y).y
  else {
    val maxY                = edges.maxBy(_.y).y
    val shape               = Shape.make(shapes.head, 2, maxY + 4)
    val (nextWind, stopped) = falling(shape, edges, wind)
    val newFloor = edges.groupBy(_.y).collect { case (y, row) if row.size == 7 => y }.maxOption.getOrElse(floor)
    val newState = (edges ++ stopped.locs).filter(_.y >= newFloor)

    simulate(cnt - 1, newState, shapes.tail :+ shapes.head, nextWind, newFloor, heights :+ maxY)
  }

val floor  = (0 to 6).map(x => Edge(x, 0)).toSet
val shapes = Vector("bar", "cross", "corner", "pipe", "square")

val heights = simulate(2022L, floor, shapes, input.toVector, 0, Vector())

// part 1
heights.last

// part 2
def findCycle(ds: Seq[Long]) = {
  @annotation.tailrec
  def find(idx: Int, length: Int): (Int, Int) = {
    val cycleEnd   = ds.length - length
    val slice      = ds.takeRight(length)
    val cycleStart = ds.lastIndexOfSlice(slice, cycleEnd - 1)
    if (cycleStart != -1) (cycleStart, cycleEnd)
    else find(idx + 1, length - 1)
  }
  find(0, ds.length - 1)
}

val rocks                   = 1_000_000_000_000L
val heightDeltas            = heights.sliding(2).map(h => h.last - h.head).toSeq
val (cycleStart, cycleEnds) = findCycle(heightDeltas)
val cycleLength             = cycleEnds - cycleStart
val cycleHeight             = heights(cycleEnds) - heights(cycleStart)
val cycleCount              = (rocks - cycleStart) / cycleLength
val partialCycleCount       = (rocks - cycleStart) % cycleLength

cycleCount * cycleHeight + heights(cycleStart + partialCycleCount.toInt)
