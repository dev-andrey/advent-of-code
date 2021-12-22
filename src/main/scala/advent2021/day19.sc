import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

val raw = scala.io.Source.fromResource(s"advent2021/day19.txt").getLines()

final case class Loc(x: Int, y: Int, z: Int) {
  def -(loc: Loc) = Loc(x - loc.x, y - loc.y, z - loc.z)
  def distance    = x.abs + y.abs + z.abs
}

final case class Scanner(beacons: Set[Loc], center: Loc = Loc(0, 0, 0))

val input = raw
  .filter(_.nonEmpty)
  .foldLeft(List.empty[Scanner]) {
    case (scanners, s"--- scanner ${id} ---") =>
      Scanner(Set()) :: scanners

    case (head :: tail, s"${x},${y},${z}") =>
      head.copy(beacons = head.beacons + Loc(x.toInt, y.toInt, z.toInt)) :: tail

    case (scanners, _) => scanners
  }
  .reverse

def rotations(loc: Loc, rot: Int): Loc = loc match {
  case Loc(x, y, z) =>
    Vector(
      // facing forward
      Loc(x, y, z),   // 0
      Loc(y, -x, z),  // 90
      Loc(-x, -y, z), // 180
      Loc(-y, x, z),  // 270
      // facing backwards
      Loc(-x, y, -z),
      Loc(y, x, -z),
      Loc(x, -y, -z),
      Loc(-y, -x, -z),
      // facing up
      Loc(x, z, -y),
      Loc(-y, z, -x),
      Loc(-x, z, y),
      Loc(y, z, x),
      // facing down
      Loc(x, -z, y),
      Loc(-y, -z, x),
      Loc(-x, -z, -y),
      Loc(y, -z, -x),
      // facing right
      Loc(z, y, -x),
      Loc(z, x, y),
      Loc(z, -y, x),
      Loc(z, -x, -y),
      // facing left
      Loc(-z, y, x),
      Loc(-z, x, -y),
      Loc(-z, -y, -x),
      Loc(-z, -x, y)
    )(rot)
}

@tailrec
def alignScanners(remaining: Vector[Scanner], alignedScanners: List[Scanner]): List[Scanner] = {
  def realign(beacons: Set[Loc], refs: Set[Loc]): Option[Scanner] = {
    for {
      rotated <- (0 to 23).map(rotation => beacons.map(rotations(_, rotation)))
      beacon  <- rotated
      delta   <- refs.map(beacon - _)
      adjusted = rotated.map(_ - delta)
      if (adjusted intersect refs).size >= 12
    } yield Scanner(adjusted, delta)
  }.headOption

  if (remaining.isEmpty) alignedScanners
  else
    alignedScanners
      .flatMap(aligned => realign(remaining.head.beacons, aligned.beacons))
      .headOption match {
      case Some(aligned) => alignScanners(remaining.tail, aligned :: alignedScanners)
      case None          => alignScanners(remaining.tail :+ remaining.head, alignedScanners)
    }
}

alignScanners(input.tail.toVector, input.head :: Nil) pipe { scanners =>
  (
    scanners.flatMap(_.beacons).distinct.length, // part 1
    (for {
      sc1 <- scanners.map(_.center)
      sc2 <- scanners.map(_.center)
    } yield (sc1 - sc2).distance).max // part 2
  )
}
