val input = scala.io.Source.fromResource(s"advent2021/day05.txt").getLines().toList

val line = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r

final case class Point(x: Int, y: Int)

input
  .map { case line(x1, y1, x2, y2) =>
    Point(x1.toInt, y1.toInt) -> Point(x2.toInt, y2.toInt)
  }
  .flatMap {
    case Point(x1, y1) -> Point(x2, y2) if x1 == x2 =>
      ((y1 min y2) to (y1 max y2)).map(Point(x1, _))

    case Point(x1, y1) -> Point(x2, y2) if y1 == y2 =>
      ((x1 min x2) to (x1 max x2)).map(Point(_, y1))

    case _ => Vector()
  }
  .groupMapReduce(identity)(_ => 1)(_ + _)
  .count(_._2 > 1)

input
  .map { case line(x1, y1, x2, y2) =>
    Point(x1.toInt, y1.toInt) -> Point(x2.toInt, y2.toInt)
  }
  .flatMap {
    case Point(x1, y1) -> Point(x2, y2) if x1 == x2 =>
      ((y1 min y2) to (y1 max y2)).map(Point(x1, _))

    case Point(x1, y1) -> Point(x2, y2) if y1 == y2 =>
      ((x1 min x2) to (x1 max x2)).map(Point(_, y1))

    case Point(x1, y1) -> Point(x2, y2) if x1 < x2 && y1 < y2 =>
      ((x1 to x2) zip (y1 to y2)).map { case (x, y) => Point(x, y) }

    case Point(x1, y1) -> Point(x2, y2) if x1 < x2 && y1 > y2 =>
      ((x1 to x2) zip (y2 to y1).reverse).map { case (x, y) => Point(x, y) }

    case Point(x1, y1) -> Point(x2, y2) if x1 > x2 && y1 < y2 =>
      ((x2 to x1).reverse zip (y1 to y2)).map { case (x, y) => Point(x, y) }

    case Point(x1, y1) -> Point(x2, y2) if x1 > x2 && y1 > y2 =>
      ((x2 to x1).reverse zip (y2 to y1).reverse).map { case (x, y) => Point(x, y) }

  }
  .groupMapReduce(identity)(_ => 1)(_ + _)
  .count(_._2 > 1)
