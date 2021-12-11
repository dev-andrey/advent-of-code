import scala.annotation.tailrec

val raw = scala.io.Source.fromResource(s"advent2021/day11.txt").getLines()

final case class Octo(x: Int, y: Int)

val (maxRow, maxCol, input) = raw.foldLeft((0, 0, Map.empty[(Int, Int), Int])) { case (row, _, grid) -> line =>
  (row + 1, line.length, grid ++ line.zipWithIndex.map(_.swap).map { case (x, ch) => (x, row) -> ch.asDigit })
}
val maxX = maxCol - 1
val maxY = maxRow - 1

val all = for {
  y <- 0 to maxY
  x <- 0 to maxX
} yield (x, y)

def draw(octos: Map[(Int, Int), Int]): Unit =
  all.foreach { case (x, y) =>
    print(s"${octos(x, y)}")
    if (x == maxX) print("\n")
  }

def adjacent(loc: (Int, Int)): Vector[(Int, Int)] = {
  for {
    dx <- -1 to 1
    dy <- -1 to 1
  } yield (loc._1 + dx, loc._2 + dy)
}.filter { case (x, y) =>
  x >= 0 && x <= maxX && y >= 0 && y <= maxY
}.toVector

@tailrec
def keepCharging(octos: Map[(Int, Int), Int], flashedBefore: Set[(Int, Int)]): Map[(Int, Int), Int] = {
  val charged = octos.groupMap(_._2)(_._1).map { case (charge, loc) => (charge, loc.toVector) }

  if (charged.keySet.contains(10)) {
    val justFlashed = charged(10).toSet
    val extraCharge = charged(10)
      .flatMap(adjacent)
      .filterNot(justFlashed)
      .filterNot(flashedBefore)
      .groupMapReduce(identity)(_ => 1)(_ + _)

    val updated = octos.map { case (loc, charge) => (loc, 10 min (charge + extraCharge.getOrElse(loc, 0))) }
    keepCharging(updated, flashedBefore ++ justFlashed)

  } else {
    println("no more")
    octos ++ flashedBefore.map(_ -> 0)
  }
}

@tailrec
def cycle(remain: Int, octos: Map[(Int, Int), Int], flashes: Int): Int =
  if (remain == 0) flashes
  else {
    println(s"step ${100 - remain}")
    draw(octos)

    val charged = keepCharging(octos.view.mapValues(_ + 1).toMap, Set())

    cycle(remain - 1, charged, flashes + charged.count(_._2 == 0))
  }

cycle(3, input, 0)
