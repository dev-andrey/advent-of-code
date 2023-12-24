val input = scala.io.Source.fromResource(s"advent2023/day24.txt").getLines().toSeq

case class Hail(px: Long, py: Long, pz: Long, vx: Long, vy: Long, vz: Long):
  val a: Double = vy
  val b: Double = -vx
  val c: Double = vy * px - vx * py

  def collide(other: Hail): Option[(Double, Double)] =
    if this parallelTo other then None
    else
      Some(
        (
          (c * other.b - other.c * b) / (a * other.b - other.a * b),
          (other.c * a - c * other.a) / (a * other.b - other.a * b)
        )
      )

  def parallelTo(other: Hail) = a * other.b == b * other.a

val hailstones = input.map { case s"${px}, ${py}, ${pz} @ ${vx}, ${vy}, ${vz}" =>
  Hail(px.trim.toLong, py.trim.toLong, pz.trim.toLong, vx.trim.toLong, vy.trim.toLong, vz.trim.toLong)
}

val low  = 200_000_000_000_000L
val high = 400_000_000_000_000L

hailstones
  .foldLeft((Seq.empty[Hail], Seq.empty[((Hail, Hail), Option[(Double, Double)])])) {
    case ((checked, intersect), hail) =>
      val found = checked.map(h => ((h, hail), h.collide(hail)))
      (checked :+ hail, intersect ++ found)
  }
  ._2
  .count {
    case ((h1, h2), Some(x, y)) if low <= x && x <= high && low <= y && y <= high =>
      (x - h1.px) * h1.vx >= 0
      && (y - h1.py) * h1.vy >= 0
      && (x - h2.px) * h2.vx >= 0
      && (y - h2.py) * h2.vy >= 0

    case _ => false
  }

// solving through sympy 769840447420960
// {vxr: -228, vyr: 166, vzr: 245, xr: 404422374079783, yr: 199182431001928, zr: 166235642339249}
// research breeze and/or apache.commons.math3
// import breeze.linalg.*
