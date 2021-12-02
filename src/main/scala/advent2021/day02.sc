val raw = scala.io.Source.fromResource(s"advent2021/day02.txt").getLines()

val steps = raw
  .map(_.split(' '))
  .map(ins => ins.head -> ins.last.toInt)
  .toList

steps
  .foldLeft((0, 0)) {
    case ((pos, depth), ("forward", x)) => (pos + x, depth)
    case ((pos, depth), ("down", x))    => (pos, depth + x)
    case ((pos, depth), ("up", x))      => (pos, depth - x)
  } match {
  case (pos, depth) => pos * depth
}

steps
  .foldLeft((0, 0, 0)) {
    case ((pos, depth, aim), ("forward", x)) => (pos + x, depth + aim * x, aim)
    case ((pos, depth, aim), ("down", x))    => (pos, depth, aim + x)
    case ((pos, depth, aim), ("up", x))      => (pos, depth, aim - x)
  } match {
  case (pos, depth, _) => pos * depth
}
