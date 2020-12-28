val input = scala.io.Source.fromResource(s"advent2015/day02.txt").getLines()

final case class Package(length: Int, width: Int, height: Int) {
  def surface: Int      = 2 * length * width + 2 * width * height + 2 * height * length
  def smallestSide: Int = List(length, width, height).sorted.take(2).product

  def bowLength = length * width * height
  def ribbon    = List(length, width, height).sorted.take(2).sum * 2
}

val packages = input.map { lwh =>
  lwh.split('x').map(_.toInt) match {
    case arr => Package(arr(0), arr(1), arr(2))
  }
}.toVector

// part 1 - wrapping
packages.map(pkg => pkg.surface + pkg.smallestSide).sum

// part 2 - ribbon
packages.map(pkg => pkg.bowLength + pkg.ribbon).sum
