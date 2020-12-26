val input = scala.io.Source.fromResource(s"advent2020/day18.txt").getLines()

sealed trait Expr
object Expr {
  final case class Num(value: Long)              extends Expr
  final case class Add(left: Expr, right: Expr)  extends Expr
  final case class Mult(left: Expr, right: Expr) extends Expr
}
import Expr.{Add, Mult, Num}

def parse(expr: String): Option[Expr] = {
  @annotation.tailrec
  def precedenceOver(str: String, idx: Int = 0, count: Int = 0): Boolean =
    if (idx == str.length) true
    else if (str(idx) == '*' && count == 0) false
    else
      str(idx) match {
        case '(' => precedenceOver(str, idx + 1, count + 1)
        case ')' => precedenceOver(str, idx + 1, count - 1)
        case _   => precedenceOver(str, idx + 1, count)
      }

  @annotation.tailrec
  def parseOperands(str: String, idx: Int, parens: Int = 0): Option[Expr] = str(idx) match {
    case '+' if parens == 0 && precedenceOver(str.substring(0, idx)) =>
      for {
        left  <- parse(str.substring(0, idx))
        right <- parse(str.substring(idx + 1))
      } yield Add(left, right)

    case '*' if parens == 0 =>
      for {
        left  <- parse(str.substring(0, idx))
        right <- parse(str.substring(idx + 1))
      } yield Mult(left, right)

    case '(' => parseOperands(str, idx - 1, parens + 1)
    case ')' => parseOperands(str, idx - 1, parens - 1)
    case _   => parseOperands(str, idx - 1, parens)
  }

  @annotation.tailrec
  def isValid(str: String, idx: Int = 0, count: Int = 0): Boolean =
    if (count < 0) false
    else if (idx == str.length) true
    else
      str(idx) match {
        case '(' => isValid(str, idx + 1, count + 1)
        case ')' => isValid(str, idx + 1, count - 1)
        case _   => isValid(str, idx + 1, count)
      }

  if (expr.contains(" ")) parse(expr.replace(" ", ""))
  else if (expr.head == '(' && expr.last == ')' && isValid(expr.tail.init)) parse(expr.tail.init)
  else if (expr.contains("*") || expr.contains("+")) parseOperands(expr, expr.length - 1)
  else Option(Num(expr.toLong))
}

def eval(expr: Expr): Long =
  expr match {
    case Expr.Num(value)        => value
    case Expr.Add(left, right)  => eval(left) + eval(right)
    case Expr.Mult(left, right) => eval(left) * eval(right)
  }

input.flatMap(parse).map(eval).sum
