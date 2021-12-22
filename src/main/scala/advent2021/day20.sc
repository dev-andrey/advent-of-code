import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

val raw = scala.io.Source.fromResource(s"advent2021/day20.txt").getLines().toList

final case class Pixel(x: Int, y: Int) { self =>
  def surround: Vector[Pixel] =
    (for {
      dy <- -1 to 1
      dx <- -1 to 1
    } yield Pixel(x + dx, y + dy)).toVector

  def isOutside(x1: Int, y1: Int)(x2: Int, y2: Int) =
    self.x < x1 || self.x > x2 || self.y < y1 || self.y > y2
}

val algo = raw.head
val (maxRow, maxCol, pixels) = raw.drop(2).foldLeft((0, 0, Set.empty[Pixel])) { case (row, _, pixels) -> line =>
  (row + 1, line.length, pixels ++ line.zipWithIndex.collect { case ('#', col) => Pixel(col, row) })
}
val minX = 0
val minY = 0
val maxX = maxCol - 1
val maxY = maxRow - 1

@tailrec
def enhance(cycle: Int, minX: Int, minY: Int, maxX: Int, maxY: Int, pixels: Set[Pixel]): Int =
  if (cycle == 0) pixels.size
  else {
    def isPixelLight(pixel: Pixel) =
      pixels.contains(pixel) || (cycle % 2 != 0 && pixel.isOutside(minX, minY)(maxX, maxY))

    val updated = for {
      y <- minY - 2 to maxY + 2
      x <- minX - 2 to maxX + 2
      if Pixel(x, y)
        .surround
        .map(px => if (isPixelLight(px)) '1' else '0')
        .mkString
        .pipe(binary => Integer.parseInt(binary, 2))
        .pipe(idx => algo(idx)) == '#'
    } yield Pixel(x, y)

    val (minx, maxx) = updated.map(_.x).sorted pipe { xs => (xs.head, xs.last) }
    val (miny, maxy) = updated.map(_.y).sorted pipe { ys => (ys.head, ys.last) }

    enhance(cycle - 1, minx, miny, maxx, maxy, updated.toSet)
  }

enhance(50, minX, minY, maxX, maxY, pixels)
