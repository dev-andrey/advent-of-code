val input = scala.io.Source.fromResource(s"advent2015/day01.txt").toVector

// part 1

input.foldLeft(0) {
  case floor -> '(' => floor + 1
  case floor -> ')' => floor - 1
}

// part 2

@annotation.tailrec
def basementAt(pos: Int, floor: Int): Int =
  if (floor == -1) pos
  else basementAt(pos + 1, if (input(pos) == '(') floor + 1 else floor - 1)

basementAt(0, 0)
