val input = scala.io.Source.fromResource(s"advent2020/day06.txt").getLines().foldLeft(Vector(Vector.empty[String])) {
  case out -> line if line.isEmpty => out :+ Vector()
  case out -> line                 => out.init :+ (out.last :+ line)
}

input
  .flatMap(_.reduceLeftOption(_ concat _))
  .map(_.toSet.size)
  .sum

input
  .flatMap(_.reduceLeftOption(_ intersect _))
  .map(_.length)
  .sum
