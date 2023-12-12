val lines = scala.io.Source.fromResource(s"advent2023/day11.txt").getLines().toSeq

case class Loc(x: Long, y: Long):
  def distanceTo(other: Loc): Long = Math.abs(this.x - other.x) + Math.abs(this.y - other.y)

val galaxies =
  for
    x <- lines.head.indices
    y <- lines.indices
    if lines(y)(x) == '#'
  yield Loc(x, y)

def expand(galaxies: Seq[Loc], expandBy: Long): Seq[Loc] =
  val columns    = galaxies.map(_.x).toSet
  val xExpansion = (0L to columns.max).toSet diff columns

  val rows       = galaxies.map(_.y).toSet
  val yExpansion = (0L to rows.max).toSet diff rows

  def expandX(galaxies: Seq[Loc], expanded: Seq[Loc], expansions: Seq[Long]): Seq[Loc] =
    if expansions.isEmpty then expanded ++ galaxies
    else
      val (stable, toExpand) = galaxies.partition(_.x <= expansions.head)
      expandX(
        toExpand.map(loc => loc.copy(x = loc.x + expandBy)),
        expanded ++ stable,
        expansions.tail.map(_ + expandBy)
      )

  def expandY(galaxies: Seq[Loc], expanded: Seq[Loc], expansions: Seq[Long]): Seq[Loc] =
    if expansions.isEmpty then expanded ++ galaxies
    else
      val (stable, toExpand) = galaxies.partition(_.y <= expansions.head)
      expandY(
        toExpand.map(loc => loc.copy(y = loc.y + expandBy)),
        expanded ++ stable,
        expansions.tail.map(_ + expandBy)
      )

  val expandedX = expandX(galaxies, Seq.empty, xExpansion.toSeq.sorted)
  expandY(expandedX, Seq.empty, yExpansion.toSeq.sorted)

val part1 = expand(galaxies, 1L)
  .combinations(2)
  .map(pair => pair.head.distanceTo(pair.last))
  .sum

val part2 = expand(galaxies, 999999L)
  .combinations(2)
  .map(pair => pair.head.distanceTo(pair.last))
  .sum
