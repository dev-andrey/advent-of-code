val strategy = scala.io.Source.fromResource(s"advent2022/day02.txt").getLines().toList

val (loose, draw, win) = (0, 3, 6)

def shapeScore(shape: Char) = shape match {
  case 'A' | 'X' => 1 // rock
  case 'B' | 'Y' => 2 // paper
  case 'C' | 'Z' => 3 // scissors
}

strategy
  .map(play => (play.head, play.last))
  .map {
    case ('A', 'X') => shapeScore('X') + draw
    case ('A', 'Y') => shapeScore('Y') + win
    case ('A', 'Z') => shapeScore('Z') + loose

    case ('B', 'X') => shapeScore('X') + loose
    case ('B', 'Y') => shapeScore('Y') + draw
    case ('B', 'Z') => shapeScore('Z') + win

    case ('C', 'X') => shapeScore('X') + win
    case ('C', 'Y') => shapeScore('Y') + loose
    case ('C', 'Z') => shapeScore('Z') + draw
  }
  .sum

// X - loose, Y - draw, Z - win
strategy
  .map(play => (play.head, play.last))
  .map {
    case ('A', 'X') => shapeScore('C') + loose
    case ('B', 'X') => shapeScore('A') + loose
    case ('C', 'X') => shapeScore('B') + loose

    case ('A', 'Y') => shapeScore('A') + draw
    case ('B', 'Y') => shapeScore('B') + draw
    case ('C', 'Y') => shapeScore('C') + draw

    case ('A', 'Z') => shapeScore('B') + win
    case ('B', 'Z') => shapeScore('C') + win
    case ('C', 'Z') => shapeScore('A') + win
  }
  .sum
