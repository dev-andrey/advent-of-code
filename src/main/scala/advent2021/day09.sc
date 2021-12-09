val raw = scala.io.Source.fromResource(s"advent2021/day09.txt").getLines()

val input = raw.foldLeft((0, Map.empty[(Int, Int), Int])) { case (row, grid) -> line =>
  (row + 1, grid ++ line.zipWithIndex.map(_.swap).map { case (x, ch) => (x, row) -> ch.asDigit })
}._2
