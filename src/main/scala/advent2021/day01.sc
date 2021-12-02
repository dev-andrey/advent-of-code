val depths = scala.io.Source.fromResource(s"advent2021/day01.txt").getLines().map(_.toInt).toList

depths.sliding(2).count(window => window.head < window.last)

depths.sliding(3).map(_.sum).sliding(2).count(window => window.head < window.last)
