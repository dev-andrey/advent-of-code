val raw = "target area: x=117..164, y=-140..-89"

final case class Loc(x: Int, y: Int)

val (minX, maxX, minY, maxY) = raw match {
  case s"target area: x=${x1}..${x2}, y=${y1}..${y2}" =>
    (x1.toInt min x2.toInt, x1.toInt max x2.toInt, y1.toInt min y2.toInt, y1.toInt max y2.toInt)
}

final case class Probe(loc: Loc, dx: Int, dy: Int, highest: Int) {
  def move = Probe(
    loc = Loc(loc.x + dx, loc.y + dy),
    dx = if (dx == 0) 0 else if (dx < 0) dx + 1 else dx - 1,
    dy = dy - 1,
    highest = highest max (loc.y + dy)
  )

  def missed: Boolean = loc.x > maxX || (loc.x < minX && dx == 0) || loc.y < minY

  def hit: Boolean = loc.x >= minX && loc.x <= maxX && loc.y >= minY && loc.y <= maxY
}

val probes = for {
  dy <- -200 to 200
  dx <- 0 to 200
} yield Probe(Loc(0, 0), dx, dy, 0)

probes.size

def simulate(probes: Seq[Probe], count: Int, maxHeight: Int): (Int, Int) =
  if (probes.isEmpty) (maxHeight, count)
  else {
    val (hitProbes, flyingProbes) = probes.map(_.move).filterNot(_.missed).partition(_.hit)
    simulate(
      flyingProbes,
      count + hitProbes.size,
      hitProbes.maxByOption(_.highest).map(_.highest).getOrElse(0) max maxHeight
    )
  }

val (maxHeight, count) = simulate(probes, 0, 0)
maxHeight
count
