val input = scala.io.Source.fromResource(s"advent2022/day22.txt").getLines().toSeq

final case class Loc(x: Int, y: Int) {
  def up    = Loc(x, y - 1)
  def right = Loc(x + 1, y)
  def down  = Loc(x, y + 1)
  def left  = Loc(x - 1, y)
}

val (_, walls, openings) =
  input.dropRight(2).foldLeft((0, Set.empty[Loc], Set.empty[Loc])) { case ((y, walls, open), line) =>
    val (wall, free) = line.zipWithIndex.partition(_._1 == '#')
    (y + 1, walls ++ wall.map(w => Loc(w._2, y)), open ++ free.filter(_._1 == '.').map(f => Loc(f._2, y)))
  }

val path = input.last

sealed trait Facing
object Facing {
  case object Up    extends Facing
  case object Right extends Facing
  case object Down  extends Facing
  case object Left  extends Facing
}

final case class Human(loc: Loc, facing: Facing) { self =>
  def turnLeft = self.facing match {
    case Facing.Up    => self.copy(facing = Facing.Left)
    case Facing.Right => self.copy(facing = Facing.Up)
    case Facing.Down  => self.copy(facing = Facing.Right)
    case Facing.Left  => self.copy(facing = Facing.Down)
  }

  def turnRight = self.facing match {
    case Facing.Up    => self.copy(facing = Facing.Right)
    case Facing.Right => self.copy(facing = Facing.Down)
    case Facing.Down  => self.copy(facing = Facing.Left)
    case Facing.Left  => self.copy(facing = Facing.Up)
  }

}
val startY = 0
val startX = openings.filter(_.y == startY).minBy(_.x).x

def movingFlat(human: Human, steps: Int): Human = {
  def up(loc: Loc) =
    if (openings.contains(loc.up) || walls.contains(loc.up)) loc.up
    else loc.copy(y = (walls ++ openings).filter(_.x == loc.x).maxBy(_.y).y)

  def right(loc: Loc) =
    if (openings.contains(loc.right) || walls.contains(loc.right)) loc.right
    else loc.copy(x = (walls ++ openings).filter(_.y == loc.y).minBy(_.x).x)

  def down(loc: Loc) =
    if (openings.contains(loc.down) || walls.contains(loc.down)) loc.down
    else loc.copy(y = (walls ++ openings).filter(_.x == loc.x).minBy(_.y).y)

  def left(loc: Loc) =
    if (openings.contains(loc.left) || walls.contains(loc.left)) loc.left
    else loc.copy(x = (walls ++ openings).filter(_.y == loc.y).maxBy(_.x).x)

  val newLoc = human.facing match {
    case Facing.Up    => up(human.loc)
    case Facing.Right => right(human.loc)
    case Facing.Down  => down(human.loc)
    case Facing.Left  => left(human.loc)
  }

  if (steps == 0 || walls.contains(newLoc)) human
  else movingFlat(human.copy(loc = newLoc), steps - 1)
}

def follow(human: Human, path: String, move: (Human, Int) => Human): Human =
  // println(s"$human  ::: $path")
  if (path.isEmpty) human
  else if (path.head == 'R') follow(human.turnRight, path.tail, move)
  else if (path.head == 'L') follow(human.turnLeft, path.tail, move)
  else follow(move(human, path.takeWhile(_.isDigit).toInt), path.dropWhile(_.isDigit), move)

follow(Human(Loc(startX, startY), Facing.Right), path, movingFlat) match {
  case Human(Loc(x, y), Facing.Right) => 1000 * (y + 1) + 4 * (x + 1) + 0
  case Human(Loc(x, y), Facing.Down)  => 1000 * (y + 1) + 4 * (x + 1) + 1
  case Human(Loc(x, y), Facing.Left)  => 1000 * (y + 1) + 4 * (x + 1) + 2
  case Human(Loc(x, y), Facing.Up)    => 1000 * (y + 1) + 4 * (x + 1) + 3
}

def moving3d(human: Human, steps: Int): Human = {
  val Human(loc @ Loc(x, y), facing) = human

  val newHuman = facing match {
    case Facing.Right if x == 50 - 1 && y >= 150 && y < 200  => Human(Loc(50 + y - 150, 150 - 1), Facing.Up)
    case Facing.Right if x == 100 - 1 && y >= 100 && y < 150 => Human(Loc(150 - 1, 50 - 1 - y + 100), Facing.Left)
    case Facing.Right if x == 150 - 1 && y >= 0 && y < 50    => Human(Loc(100 - 1, 150 - 1 - y), Facing.Left)
    case Facing.Right if x == 100 - 1 && y >= 50 && y < 100  => Human(Loc(100 + y - 50, 50 - 1), Facing.Up)
    case Facing.Right                                        => Human(loc.right, facing)

    case Facing.Down if y == 50 - 1 && x >= 100 && x < 150 => Human(Loc(100 - 1, 50 + x - 100), Facing.Left)
    case Facing.Down if y == 150 - 1 && x >= 50 && x < 100 => Human(Loc(50 - 1, 150 + x - 50), Facing.Left)
    case Facing.Down if y == 200 - 1 && x >= 0 && x < 50   => Human(Loc(100 + x, 0), Facing.Down)
    case Facing.Down                                       => Human(loc.down, facing)

    case Facing.Left if x == 0 && y >= 150 && y < 200 => Human(Loc(50 + y - 150, 0), Facing.Down)
    case Facing.Left if x == 50 && y >= 0 && y < 50   => Human(Loc(0, 150 - 1 - y), Facing.Right)
    case Facing.Left if x == 0 && y >= 100 && y < 150 => Human(Loc(50, 50 - 1 - y + 100), Facing.Right)
    case Facing.Left if x == 50 && y >= 50 && y < 100 => Human(Loc(y - 50, 100), Facing.Down)
    case Facing.Left                                  => Human(loc.left, facing)

    case Facing.Up if y == 0 && x >= 50 && x < 100  => Human(Loc(0, 150 + x - 50), Facing.Right)
    case Facing.Up if y == 100 && x >= 0 && x < 50  => Human(Loc(50, 50 + x), Facing.Right)
    case Facing.Up if y == 0 && x >= 100 && x < 150 => Human(Loc(x - 100, 200 - 1), Facing.Up)
    case Facing.Up                                  => Human(loc.up, facing)

  }

  // useful during debugging:
  if (!(walls ++ openings).contains(newHuman.loc)) throw new RuntimeException(s"BAD $human -> $newHuman")

  if (steps == 0 || walls.contains(newHuman.loc)) human
  else moving3d(newHuman, steps - 1)
}

follow(Human(Loc(startX, startY), Facing.Right), path, moving3d) match {
  case Human(Loc(x, y), Facing.Right) => 1000 * (y + 1) + 4 * (x + 1) + 0
  case Human(Loc(x, y), Facing.Down)  => 1000 * (y + 1) + 4 * (x + 1) + 1
  case Human(Loc(x, y), Facing.Left)  => 1000 * (y + 1) + 4 * (x + 1) + 2
  case Human(Loc(x, y), Facing.Up)    => 1000 * (y + 1) + 4 * (x + 1) + 3
}
