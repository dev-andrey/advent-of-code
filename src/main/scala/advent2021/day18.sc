val raw = scala.io.Source.fromResource(s"advent2021/day18.txt").getLines()

sealed trait Snail
object Snail {
  final case class Pair(left: Snail, right: Snail) extends Snail
  final case class Regular(num: Int)               extends Snail
}

import Snail._
def parse(line: String): Snail = {
  val regular = "(\\d)".r

  @scala.annotation.tailrec
  def findSplit(idx: Int, stack: List[Char], str: String): (String, String) =
    if (str(idx) == ',' && stack.isEmpty) (str.take(idx), str.drop(idx + 1))
    else if (str(idx) == '[') findSplit(idx + 1, str(idx) :: stack, str)
    else if (str(idx) == ']') findSplit(idx + 1, stack.tail, str)
    else findSplit(idx + 1, stack, str)

  line match {
    case s"[$pair]" =>
      val (left, right) = findSplit(0, List(), pair)
      Pair(parse(left), parse(right))
    case regular(num) => Regular(num.toInt)
  }
}

val input = raw.map(parse).toVector

def explode(snail: Snail): (Snail, Boolean) = {
  def update(curr: Snail, addToLeft: Int, addToRight: Int): Snail =
    curr match {
      case Pair(left, right) => Pair(update(left, addToLeft, 0), update(right, 0, addToRight))
      case Regular(num)      => Regular(num + addToLeft + addToRight)
    }

  def explode(curr: Snail, lvl: Int, exploded: Boolean): (Snail, Int, Int, Boolean) =
    curr match {
      case Pair(Regular(left), Regular(right)) if lvl >= 4 && !exploded =>
        (Regular(0), left, right, true)

      case Pair(left, right) =>
        val (lSnail, lLeft, lRight, lExploded) = explode(left, lvl + 1, exploded)
        val (rSnail, rLeft, rRight, rExploded) = explode(right, lvl + 1, lExploded)
        (Pair(update(lSnail, 0, rLeft), update(rSnail, lRight, 0)), lLeft, rRight, rExploded)

      case Regular(num) =>
        (Regular(num), 0, 0, exploded)
    }

  val (result, _, _, exploded) = explode(snail, 0, exploded = false)
  (result, exploded)
}

def split(snail: Snail): (Snail, Boolean) = {
  def doSplit(curr: Snail, split: Boolean): (Snail, Boolean) =
    curr match {
      case Regular(num) if num >= 10 && !split =>
        (Pair(Regular(num / 2), Regular(num - (num / 2))), true)

      case Pair(left, right) =>
        val (lSnail, leftSplit)  = doSplit(left, split)
        val (rSnail, rightSplit) = doSplit(right, leftSplit)
        (Pair(lSnail, rSnail), rightSplit)

      case Regular(num) =>
        (Regular(num), split)
    }

  doSplit(snail, split = false)
}

@scala.annotation.tailrec
def reduce(snail: Snail): Snail =
  explode(snail) match {
    case (exploded, true) => reduce(exploded)
    case (notExploded, _) =>
      split(notExploded) match {
        case (split, true) => reduce(split)
        case (notSplit, _) => notSplit
      }
  }

def add(left: Snail, right: Snail): Snail =
  reduce(Pair(left, right))

def magnitude(snail: Snail): Int =
  snail match {
    case Pair(left, right) => 3 * magnitude(left) + 2 * magnitude(right)
    case Regular(num)      => num
  }

// part 1
magnitude(input.reduce(add))

// part 2
(for {
  snail1 <- input
  snail2 <- input
  if snail1 != snail2
  magnitude <- List(magnitude(add(snail1, snail2)), magnitude(add(snail2, snail1)))
} yield magnitude).max
