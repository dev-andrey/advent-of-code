val input = scala.io.Source.fromResource(s"advent2023/day22.txt").getLines().toSeq

case class Pos(x: Int, y: Int, z: Int)

case class Brick(p1: Pos, p2: Pos):
  def fallBy(distance: Int) =
    this.copy(p1 = p1.copy(z = distance), p2 = p2.copy(z = p2.z - (p1.z - distance)))

  def overlaps(other: Brick) =
    math.max(this.p1.x, other.p1.x) <= math.min(this.p2.x, other.p2.x)
      && math.max(this.p1.y, other.p1.y) <= math.min(this.p2.y, other.p2.y)

val bricks = input
  .map { case s"${x1},${y1},${z1}~${x2},${y2},${z2}" =>
    Brick(Pos(x1.toInt, y1.toInt, z1.toInt), Pos(x2.toInt, y2.toInt, z2.toInt))
  }
  .sortBy(_.p1.z)

val fallen = bricks
  .foldLeft(Seq.empty[Brick]) { case (fallen, brick) =>
    val fallDistance = fallen.filter(_.overlaps(brick)).map(_.p2.z + 1).maxOption.getOrElse(1)
    fallen :+ brick.fallBy(fallDistance)
  }
  .sortBy(_.p1.z)

val supporting = for
  upper <- fallen
  lower <- fallen
  if lower.overlaps(upper) && upper.p1.z == lower.p2.z + 1
yield lower -> upper

val topBricks   = fallen.toSet -- supporting.map(_._1).toSet
val supportedBy = supporting.groupMap(_._1)(_._2) ++ topBricks.map(_ -> Seq())

val bottomBricks = supportedBy.keys.toSet -- supporting.map(_._2).toSet
val supports     = supporting.groupMap(_._2)(_._1) ++ bottomBricks.map(_ -> Seq())

val safeToKill = fallen.filter { brick =>
  supportedBy(brick).forall(supportedBrick => supports(supportedBrick).exists(_ != brick))
}.toSet

val part1 = safeToKill.size

def countFall(bricks: Seq[Brick], total: Int): Int =

  def inner(q: Seq[Brick], falling: Set[Brick]): Int =
    if q.isEmpty then falling.size - 1
    else
      val next = (supportedBy(q.head).toSet -- falling).filter(b => supports(b).forall(falling))
      inner(q.tail ++ next, falling ++ next)

  if bricks.isEmpty then total
  else
    val q        = supportedBy(bricks.head).filter(b => supports(b).size == 1)
    val subtotal = inner(q, q.toSet + bricks.head)
    countFall(bricks.tail, total + subtotal)

val part2 = countFall(fallen.filterNot(safeToKill), 0)
