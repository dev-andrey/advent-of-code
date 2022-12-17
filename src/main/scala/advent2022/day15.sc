val input = scala.io.Source.fromResource(s"advent2022/day15.txt").getLines().toSeq
final case class Loc(x: Int, y: Int) { self =>
  def distanceTo(other: Loc): Int = (self.x - other.x).abs + (self.y - other.y).abs
}

val objects = input.map { case s"Sensor at x=$x, y=$y: closest beacon is at x=$bX, y=$bY" =>
  Loc(x.toInt, y.toInt) -> Loc(bX.toInt, bY.toInt)
}.toSet

// part 1
//val lineDepth = 2_000_000
val lineDepth = 10 //

objects.flatMap { case (sensor, beacon) =>
  val distance = sensor.distanceTo(beacon)
  ((sensor.x - distance) to (sensor.x + distance))
    .map(x => Loc(x, lineDepth))
    .filter(loc => loc != beacon && sensor.distanceTo(loc) <= distance)
}.size

// part 2 (Ugly bruteforce, needs 4Gb of heap and 45 sec, don't run as scala worksheet, create an object)
//val maxLoc = 4_000_000
val maxLoc = 20
def outside(center: Loc, distance: Int) =
  (0 to distance)
    .foldLeft((Set.empty[Loc], distance)) { case ((acc, dy), d) =>
      (
        acc ++ Set(
          Loc(center.x - d, center.y - dy),
          Loc(center.x - d, center.y + dy),
          Loc(center.x + d, center.y - dy),
          Loc(center.x + d, center.y + dy)
        ).filter(loc => loc.x >= 0 && loc.y >= 0 && loc.x <= maxLoc & loc.y <= maxLoc),
        dy - 1
      )
    }
    ._1

objects
  .flatMap { case (sensor, beacon) =>
    outside(sensor, sensor.distanceTo(beacon) + 1)
  }
  .filter { loc =>
    objects.forall { case (sensor, beacon) => sensor.distanceTo(loc) > sensor.distanceTo(beacon) }
  }
  .map { case Loc(x, y) => x * 4_000_000L + y }
