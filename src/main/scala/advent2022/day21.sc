val input = scala.io.Source.fromResource(s"advent2022/day21.txt").getLines().toList

sealed trait Monkey

object Monkey {
  final case class Yelling(num: Long)                       extends Monkey
  final case class Adding(left: String, right: String)      extends Monkey
  final case class Subtracting(left: String, right: String) extends Monkey
  final case class Multiplying(left: String, right: String) extends Monkey
  final case class Dividing(left: String, right: String)    extends Monkey
}

val monkeys = input.map {
  case s"$name: $left + $right" => name -> Monkey.Adding(left, right)
  case s"$name: $left - $right" => name -> Monkey.Subtracting(left, right)
  case s"$name: $left * $right" => name -> Monkey.Multiplying(left, right)
  case s"$name: $left / $right" => name -> Monkey.Dividing(left, right)
  case s"$name: $num"           => name -> Monkey.Yelling(num.toLong)
}.toMap

def memo[K, V](fn: K => V): K => V = {
  val cache = collection.mutable.Map.empty[K, V]
  key => cache.getOrElseUpdate(key, fn(key))
}

lazy val answer: String => Long = memo { monkey =>
  monkeys(monkey) match {
    case Monkey.Yelling(num)             => num
    case Monkey.Adding(left, right)      => answer(left) + answer(right)
    case Monkey.Subtracting(left, right) => answer(left) - answer(right)
    case Monkey.Multiplying(left, right) => answer(left) * answer(right)
    case Monkey.Dividing(left, right)    => answer(left) / answer(right)
  }
}

answer("root")

val humans = monkeys
  .removed("humn")
  .updatedWith("root") { case Some(Monkey.Adding(left, right)) =>
    Option(Monkey.Subtracting(left, right))
  }

lazy val searching: String => Option[Long] = memo { monkey =>
  if (monkey == "humn") None
  else
    humans(monkey) match {
      case Monkey.Yelling(num)             => Some(num)
      case Monkey.Adding(left, right)      => (searching(left) zip searching(right)).map { case (l, r) => l + r }
      case Monkey.Subtracting(left, right) => (searching(left) zip searching(right)).map { case (l, r) => l - r }
      case Monkey.Multiplying(left, right) => (searching(left) zip searching(right)).map { case (l, r) => l * r }
      case Monkey.Dividing(left, right)    => (searching(left) zip searching(right)).map { case (l, r) => l / r }
    }
}

def makeMonkey(name: String)(goal: Long): Option[Long] = {
  def invertMonkey(monkey: Monkey, goal: Long): Option[Long] =
    monkey match {
      case Monkey.Yelling(_) => None
      case Monkey.Adding(left, right) =>
        (searching(left), searching(right)) match {
          case (Some(l), None) => makeMonkey(right)(goal - l)
          case (None, Some(r)) => makeMonkey(left)(goal - r)
        }
      case Monkey.Subtracting(left, right) =>
        (searching(left), searching(right)) match {
          case (Some(l), None) => makeMonkey(right)(l - goal)
          case (None, Some(r)) => makeMonkey(left)(r + goal)
        }
      case Monkey.Multiplying(left, right) =>
        (searching(left), searching(right)) match {
          case (Some(l), None) => makeMonkey(right)(goal / l)
          case (None, Some(r)) => makeMonkey(left)(goal / r)
        }
      case Monkey.Dividing(left, right) =>
        (searching(left), searching(right)) match {
          case (Some(l), None) => makeMonkey(right)(goal * l)
          case (None, Some(r)) => makeMonkey(left)(goal * r)
        }
    }

  if (name == "humn") Option(goal)
  else invertMonkey(humans(name), goal)
}

makeMonkey("root")(0L)
