val raw = scala.io.Source.fromResource(s"advent2021/day13.txt").getLines()

val (locs, actions) = raw.foldLeft(Set.empty[(Int, Int)], Vector.empty[(String, Int)]) {
  case ((locs, actions), line) if line.isEmpty         => (locs, actions)
  case ((locs, actions), s"fold along ${axis}=${num}") => (locs, actions :+ (axis, num.toInt))
  case ((locs, actions), s"$x,$y")                     => (locs + ((x.toInt, y.toInt)), actions)
}

//part 1
actions
  .take(1)
  .foldLeft(locs) {
    case (folded, ("y", offset)) =>
      folded.map {
        case (x, y) if y > offset => (x, offset - (y - offset))
        case (x, y)               => (x, y)
      }
    case (folded, ("x", offset)) =>
      folded.map {
        case (x, y) if x > offset => (offset - (x - offset), y)
        case (x, y)               => (x, y)
      }
  }
  .size

// part 2
val foldedPaper = actions.foldLeft(locs) {
  case (folded, ("y", offset)) =>
    folded.map {
      case (x, y) if y > offset => (x, offset - (y - offset))
      case (x, y)               => (x, y)
    }
  case (folded, ("x", offset)) =>
    folded.map {
      case (x, y) if x > offset => (offset - (x - offset), y)
      case (x, y)               => (x, y)
    }
}

(for {
  y <- 0 to 5
  x <- 0 to 40
} yield (x, y)).foreach { case (x, y) =>
  if (foldedPaper.contains((x, y))) print('#')
  else print(' ')
  if (x == 40) print('\n')
}
