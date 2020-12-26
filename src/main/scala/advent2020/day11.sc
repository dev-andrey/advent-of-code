val input = scala.io.Source.fromResource(s"advent2020/day11.txt").getLines().toVector.map(_.toVector)

val maxY = input.length - 1
val maxX = input.head.length - 1

@annotation.tailrec
def part1(state: Vector[Vector[Char]]): Vector[Vector[Char]] = {
  def seatsAround(x: Int, y: Int): Int =
    List(
      (x - 1, y),     // left
      (x - 1, y - 1), // top-left
      (x, y - 1),     // top
      (x + 1, y - 1), // top-right
      (x + 1, y),     // right
      (x + 1, y + 1), // bottom-right
      (x, y + 1),     // bottom
      (x - 1, y + 1)  // bottom-left
    ).count {
      case (x, y) if x >= 0 && y >= 0 && x <= maxX && y <= maxY => state(y)(x) == '#'
      case _                                                    => false
    }

  val nextState = (0 to maxY).foldLeft(Vector.empty[Vector[Char]]) { case output -> y =>
    output :+ (0 to maxX).map { x =>
      state(y)(x) match {
        case '#' if seatsAround(x, y) >= 4 => 'L'
        case 'L' if seatsAround(x, y) == 0 => '#'
        case state                         => state
      }
    }.toVector
  }

  if (nextState == state) nextState
  else part1(nextState)
}

part1(input).map(_.count(_ == '#')).sum

@annotation.tailrec
def part2(state: Vector[Vector[Char]]): Vector[Vector[Char]] = {
  def seatsOccupiedVisible(x: Int, y: Int): Int = {
    @annotation.tailrec
    def isOccupiedVisible(offset: (Int, Int))(x: Int, y: Int): Boolean =
      if (x < 0 || y < 0 || x > maxX || y > maxY) false
      else if (state(y)(x) == 'L') false
      else if (state(y)(x) == '#') true
      else isOccupiedVisible(offset)(x + offset._1, y + offset._2)

    List((x, y)).flatMap { case (x, y) =>
      List(
        isOccupiedVisible((-1, 0))(x - 1, y),      // left
        isOccupiedVisible((-1, -1))(x - 1, y - 1), // top-left
        isOccupiedVisible((0, -1))(x, y - 1),      // top
        isOccupiedVisible((1, -1))(x + 1, y - 1),  // top-right
        isOccupiedVisible((1, 0))(x + 1, y),       // right
        isOccupiedVisible((1, 1))(x + 1, y + 1),   // bottom-right
        isOccupiedVisible((0, 1))(x, y + 1),       // bottom
        isOccupiedVisible((-1, 1))(x - 1, y + 1)   // bottom-left
      )
    }.count(_ == true)
  }

  val nextState = (0 to maxY).foldLeft(Vector.empty[Vector[Char]]) { case output -> y =>
    output :+ (0 to maxX).map { x =>
      state(y)(x) match {
        case '#' if seatsOccupiedVisible(x, y) >= 5 => 'L'
        case 'L' if seatsOccupiedVisible(x, y) == 0 => '#'
        case state                                  => state
      }
    }.toVector
  }

  if (nextState == state) nextState
  else part2(nextState)
}

part2(input).map(_.count(_ == '#')).sum
