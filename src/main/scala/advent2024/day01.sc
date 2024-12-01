val lines = scala.io.Source.fromResource(s"advent2024/day01.txt").getLines().toList

val (left, right) =
  lines
    .map(_.split("\\s+").map(_.toInt))
    .map(parsed => (parsed.head, parsed.last))
    .unzip

// part 1
(left.sorted zip right.sorted).map { case (l, r) =>
  math.abs(l - r)
}.sum

// part 2
val scores = right.groupMapReduce(identity)(_ => 1)(_ + _)
left.map(l => l * scores.getOrElse(l, 0)).sum
