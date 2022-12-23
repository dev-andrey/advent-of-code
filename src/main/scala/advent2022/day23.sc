val input = scala.io.Source.fromResource(s"advent2022/day23.txt").getLines()

final case class Loc(x: Int, y: Int) { self =>
  def western  = Set(self.copy(x = x - 1, y = y + 1), self.copy(x = x - 1), self.copy(x = x - 1, y = y - 1))
  def northern = Set(self.copy(x = x - 1, y = y - 1), self.copy(y = y - 1), self.copy(x = x + 1, y = y - 1))
  def eastern  = Set(self.copy(x = x + 1, y = y - 1), self.copy(x = x + 1), self.copy(x = x + 1, y = y + 1))
  def southern = Set(self.copy(x = x - 1, y = y + 1), self.copy(y = y + 1), self.copy(x = x + 1, y = y + 1))
  def around   = eastern ++ northern ++ western ++ southern

  def +(other: Loc) = self.copy(x = x + other.x, y = y + other.y)
}

val elves = input.zipWithIndex.flatMap { case (line, y) =>
  line.zipWithIndex.collect { case '#' -> x => Loc(x, y) }
}.toSet

def draw(elves: Set[Loc]) = {
  println("\n\n\n")
  for {
    y <- elves.minBy(_.y).y to elves.maxBy(_.y).y
    x <- elves.minBy(_.x).x to elves.maxBy(_.x).x
  } yield {
    if (x == elves.minBy(_.x).x) print("\n")
    if (elves(Loc(x, y))) { print("#"); 0 }
    else { print("."); 1 }
  }
}

def simulate(round: Int, state: Set[Loc], directions: Vector[(Loc => Set[Loc], Loc)]): (Set[Loc], Int) =
  if (round == 0) (state, round)
  else {
    val (stay, move) = state.toSeq.partition(_.around.intersect(state).isEmpty)
    val (notMoving, moving) = move.foldLeft((Seq.empty[Loc], Seq.empty[(Loc, Loc)])) {
      case ((notMoving, moving), elf) =>
        directions.find { case (fn, _) => fn(elf).intersect(state).isEmpty } match {
          case Some((_, dir)) => (notMoving, moving :+ (elf, elf + dir))
          case None           => (notMoving :+ elf, moving)
        }
    }
    val (valid, conflict) = moving.groupBy(_._2).partition(_._2.size == 1)
    val moved             = valid.keys.toSet
    val revertBack        = conflict.flatMap(_._2).keys.toSet

    val newState = stay.toSet ++ notMoving.toSet ++ moved ++ revertBack

    if (newState == state) (state, round - 1)
    else simulate(round - 1, newState, directions.tail :+ directions.head)
  }

// part 1
val (result, _) = simulate(
  10,
  elves,
  Vector((_.northern, Loc(0, -1)), (_.southern, Loc(0, 1)), (_.western, Loc(-1, 0)), (_.eastern, Loc(1, 0)))
)
draw(result).sum

// part 2
val (_, round) = simulate(
  Int.MaxValue,
  elves,
  Vector((_.northern, Loc(0, -1)), (_.southern, Loc(0, 1)), (_.western, Loc(-1, 0)), (_.eastern, Loc(1, 0)))
)
Int.MaxValue - round
