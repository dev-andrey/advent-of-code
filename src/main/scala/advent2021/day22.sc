val raw = scala.io.Source.fromResource(s"advent2021/day22.txt").getLines()

final case class Loc(x: Int, y: Int, z: Int)

final case class Cuboid(isOn: Boolean, min: Loc, max: Loc) {
  def overlaps(other: Cuboid): Boolean =
    !(min.x > other.max.x || max.x < other.min.x ||
      min.y > other.max.y || max.y < other.min.y ||
      min.z > other.max.z || max.z < other.min.z)

  def negate(other: Cuboid): Cuboid =
    Cuboid(
      !isOn,
      Loc(min.x max other.min.x, min.y max other.min.y, min.z max other.min.z),
      Loc(max.x min other.max.x, max.y min other.max.y, max.z min other.max.z)
    )

  def volume: Long =
    (max.x - min.x + 1L) * (max.y - min.y + 1L) * (max.z - min.z + 1L) * (if (isOn) 1 else -1)
}

val input = raw.foldLeft(Vector.empty[Cuboid]) {
  case (steps, s"${onOff} x=${x1}..${x2},y=${y1}..${y2},z=${z1}..${z2}") =>
    steps :+ Cuboid(
      onOff == "on",
      Loc(x1.toInt min x2.toInt, y1.toInt min y2.toInt, z1.toInt min z2.toInt),
      Loc(x1.toInt max x2.toInt, y1.toInt max y2.toInt, z1.toInt max z2.toInt)
    )
}

input
  .filter(cuboid => List(cuboid.min.x, cuboid.min.y, cuboid.min.z).forall(_ >= -50))
  .filter(cuboid => List(cuboid.max.x, cuboid.max.y, cuboid.max.z).forall(_ <= 50))
  .foldLeft(Vector.empty[Cuboid]) { case (reactor, cuboid) =>
    reactor ++
      reactor.filter(_.overlaps(cuboid)).map(_.negate(cuboid)) ++
      (if (cuboid.isOn) Vector(cuboid) else Vector())
  }
  .map(_.volume)
  .sum

input
  .foldLeft(Vector.empty[Cuboid]) { case (reactor, cuboid) =>
    reactor ++
      reactor.filter(_.overlaps(cuboid)).map(_.negate(cuboid)) ++
      (if (cuboid.isOn) Vector(cuboid) else Vector())
  }
  .map(_.volume)
  .sum
