val lines = scala.io.Source.fromResource(s"advent2023/day03.txt").getLines().toList

val maxX = lines.head.length - 1
val maxY = lines.length - 1

case class Loc(x: Int, y: Int):
  def around = {
    for
      dx <- -1 to 1
      dy <- -1 to 1
    yield this.copy(x = this.x + dx, y = this.y + dy)
  }.toSet

val grid = {
  for
    x <- 0 to maxX
    y <- 0 to maxY
    if lines(y)(x) != '.'
  yield (Loc(x, y), lines(y)(x))
}.toMap

@scala.annotation.tailrec
def search(x: Int, y: Int, current: Seq[Loc], result: Seq[Int]): Seq[Int] =
  if x == maxX && y == maxY then result
  else if grid.get(Loc(x, y)).exists(_.isDigit) then search(x + 1, y, current :+ Loc(x, y), result)
  else
    val nearby  = current.flatMap(_.around).toSet diff current.toSet
    val isValid = nearby.exists(loc => grid.get(loc).exists(ch => !ch.isDigit))

    search(
      if x > maxX then 0 else x + 1,
      if x > maxX then y + 1 else y,
      Seq.empty,
      if isValid then result :+ current.map(grid).mkString.toInt else result
    )

val part1 = search(0, 0, Seq.empty, Seq.empty).sum

@scala.annotation.tailrec
def searchGear(x: Int, y: Int, current: Seq[Loc], result: Map[Loc, Seq[Int]]): Map[Loc, Seq[Int]] =
  if x == maxX && y == maxY then result
  else if grid.get(Loc(x, y)).exists(_.isDigit) then searchGear(x + 1, y, current :+ Loc(x, y), result)
  else
    val nearby       = current.flatMap(_.around).toSet diff current.toSet
    val maybeGearLoc = nearby.find(loc => grid.get(loc).contains('*'))

    searchGear(
      if x > maxX then 0 else x + 1,
      if x > maxX then y + 1 else y,
      Seq.empty,
      maybeGearLoc.fold(result) { gearLoc =>
        val num = current.map(grid).mkString.toInt
        result + (gearLoc -> (result.getOrElse(gearLoc, Seq.empty) :+ num))
      }
    )

val part2 = searchGear(0, 0, Seq.empty, Map.empty).values
  .filter(_.size == 2)
  .map(_.product)
  .sum
