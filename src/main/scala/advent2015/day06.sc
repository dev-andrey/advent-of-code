val input = scala.io.Source.fromResource(s"advent2015/day06.txt").getLines().toList

sealed trait Action
case object TurnOn  extends Action
case object TurnOff extends Action
case object Toggle  extends Action

final case class Loc(x: Int, y: Int)

val instr = "(toggle|turn\\son|turn\\soff)\\s(\\d+),(\\d+)\\sthrough\\s(\\d+),(\\d+)".r

val instructions = input.collect {
  case instr("toggle", x1, y1, x2, y2)   => (Toggle, Loc(x1.toInt, y1.toInt), Loc(x2.toInt, y2.toInt))
  case instr("turn on", x1, y1, x2, y2)  => (TurnOn, Loc(x1.toInt, y1.toInt), Loc(x2.toInt, y2.toInt))
  case instr("turn off", x1, y1, x2, y2) => (TurnOff, Loc(x1.toInt, y1.toInt), Loc(x2.toInt, y2.toInt))
}

// Part I

final case class LightGrid(get: Loc => Boolean) { self =>
  def set(topLeft: Loc, bottomRight: Loc, action: Action): LightGrid =
    LightGrid { case loc @ Loc(x, y) =>
      if (x >= topLeft.x && y >= topLeft.y && x <= bottomRight.x && y <= bottomRight.y)
        action match {
          case TurnOn  => true
          case TurnOff => false
          case Toggle  => !self.get(loc)
        }
      else self.get(loc)
    }
  override def toString = {
    for {
      x <- 0 to 999
      y <- 0 to 999
    } yield Loc(x, y)
  }.count(self.get).toString
}

instructions.foldLeft(LightGrid(_ => false)) { case (grid, (action, topLeft, bottomRight)) =>
  grid.set(topLeft, bottomRight, action)
}

// Part II

final case class BrightGrid(get: Loc => Long) { self =>
  def set(topLeft: Loc, bottomRight: Loc, action: Action): BrightGrid =
    BrightGrid { case loc @ Loc(x, y) =>
      if (x >= topLeft.x && y >= topLeft.y && x <= bottomRight.x && y <= bottomRight.y)
        action match {
          case TurnOn  => self.get(loc) + 1L
          case TurnOff => 0L max (self.get(loc) - 1L)
          case Toggle  => self.get(loc) + 2L
        }
      else self.get(loc)
    }

  override def toString = {
    for {
      x <- 0 to 999
      y <- 0 to 999
    } yield Loc(x, y)
  }.map(self.get).sum.toString
}

instructions.foldLeft(BrightGrid(_ => 0L)) { case (grid, (action, topLeft, bottomRight)) =>
  grid.set(topLeft, bottomRight, action)
}
