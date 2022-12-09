val input = scala.io.Source.fromResource(s"advent2022/day09.txt").getLines().toSeq

final case class Knot(x: Int, y: Int) { self =>
  def move(direction: String) = direction match {
    case "L" => self.copy(x = self.x - 1)
    case "U" => self.copy(y = self.y + 1)
    case "R" => self.copy(x = self.x + 1)
    case "D" => self.copy(y = self.y - 1)
  }
  def touching(other: Knot): Boolean =
    self.y == other.y && (self.x - other.x).abs <= 1 ||
      self.x == other.x && (self.y - other.y).abs <= 1 ||
      (self.x - other.x).abs <= 1 && (self.y - other.y).abs <= 1

  def verticallyFarFrom(other: Knot): Boolean =
    (self.x == other.x) && (self.y - other.y).abs == 2

  def horizontallyFarFrom(other: Knot): Boolean =
    (self.y == other.y) && (self.x - other.x).abs == 2

  def dragTo(other: Knot): Knot =
    if (self touching other) self
    else if (self horizontallyFarFrom other) self.copy(x = self.x + (if (self.x > other.x) -1 else +1))
    else if (self verticallyFarFrom other) self.copy(y = self.y + (if (self.y > other.y) -1 else +1))
    else self.copy(x = self.x + (if (self.x > other.x) -1 else +1), y = self.y + (if (self.y > other.y) -1 else +1))
}

def simulate(rope: List[Knot]): (List[Knot], Set[Knot]) =
  input
    .foldLeft((rope, Set.empty[Knot])) { case (rope, visited) -> s"$direction $steps" =>
      val (movedRope, path) = (1 to steps.toInt)
        .foldLeft((rope, Seq.empty[Knot])) { case (head :: tail, path) -> _ =>
          val movedRope = tail.foldLeft(Seq(head.move(direction))) { case movedKnots -> currKnot =>
            movedKnots :+ currKnot.dragTo(movedKnots.last)
          }
          (movedRope.toList, path :+ movedRope.last)
        }
      (movedRope, visited ++ path)
    }

// part 1
simulate(List.fill(2)(Knot(0, 0)))._2.size

// part 2
simulate(List.fill(10)(Knot(0, 0)))._2.size
