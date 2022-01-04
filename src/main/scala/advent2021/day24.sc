import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

val raw = scala.io.Source.fromResource(s"advent2021/day24.txt").mkString

val regex =
  """inp w
    |mul x 0
    |add x z
    |mod x 26
    |div z (1|26)
    |add x (-?\d+)
    |eql x w
    |eql x 0
    |mul y 0
    |add y 25
    |mul y x
    |add y 1
    |mul z y
    |mul y 0
    |add y w
    |add y (\d+)
    |mul y x
    |add z y""".stripMargin.r

/*

  X: ((z % 26 + adderX) != w)
  Z: (26 * z / (1|26) )
  Y: w + adderY
  z' =  Z + Y * X
 */

final case class Step(addToX: Int, addToY: Int)

val steps = regex
  .findAllMatchIn(raw)
  .map(found => Step(found.group(2).toInt, found.group(3).toInt))
  .toList

final case class Rule(diff: Int, i: Int, j: Int)
final case class AdderY(addToY: Int, idx: Int)

@tailrec
def make(idx: Int, steps: List[Step], stack: List[AdderY], result: List[Rule]): List[Rule] =
  if (steps.isEmpty) result
  else if (stack.nonEmpty && (0 to 9).contains((stack.head.addToY + steps.head.addToX).abs))
    make(idx + 1, steps.tail, stack.tail, Rule(stack.head.addToY + steps.head.addToX, stack.head.idx, idx) :: result)
  else make(idx + 1, steps.tail, AdderY(steps.head.addToY, idx) :: stack, result)

val rules = make(0, steps, List(), List())

rules.foldLeft(Vector.fill(steps.size)(1 to 9)) {
  case (digits, Rule(diff, i, j)) if diff > 0 =>
    digits
      .updated(i, digits(i).min to (9 - diff))
      .updated(j, (1 + diff) to digits(j).max)
  case (digits, Rule(diff, i, j)) =>
    digits
      .updated(i, (1 - diff) to digits(i).max)
      .updated(j, digits(j).min to (9 + diff))
} pipe { rules =>
  (
    rules.map(_.max).mkString, // Part 1
    rules.map(_.min).mkString  // Part 2
  )
}
