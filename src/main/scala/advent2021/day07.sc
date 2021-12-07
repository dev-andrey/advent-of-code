val raw = scala.io.Source.fromResource(s"advent2021/day07.txt").getLines()

val input = raw.flatMap(_.split(',').map(_.toInt)).toVector

// Part 1
input.indices.map(idx => input.map(pos => (pos - idx).abs).sum).min

// Part 2
input.indices.map(idx => input.map(pos => (0 to (pos - idx).abs).sum).sum).min
