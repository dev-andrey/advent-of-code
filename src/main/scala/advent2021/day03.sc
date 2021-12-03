import scala.annotation.tailrec

val raw = scala.io.Source.fromResource(s"advent2021/day03.txt").getLines().toList

implicit class VectorOps[A](vec: Vector[A]) {
  def asDecimal: Int = Integer.parseInt(vec.mkString, 2)
}

val codes = raw

val gamma = codes
  .foldLeft(Vector.fill(codes.length)(0)) { case (result, curr) =>
    (result zip curr).map {
      case (n, '1') => n + 1
      case (n, '0') => n
    }
  }
  .map {
    case n if n > codes.length / 2 => 1
    case _                         => 0
  }

// Part 1
gamma.asDecimal * gamma.map(n => if (n == 1) 0 else 1).asDecimal

// Part 2

val data = raw.map(_.toVector)

@tailrec
def oxygen(data: List[Vector[Char]], idx: Int = 0): Vector[Char] =
  if (data.length == 1) data.head
  else
    data.partition(_(idx) == '1') match {
      case (ones, zeros) if ones.length == zeros.length => oxygen(ones, idx + 1)
      case (ones, zeros) if ones.length > zeros.length  => oxygen(ones, idx + 1)
      case (_, zeros)                                   => oxygen(zeros, idx + 1)
    }

@tailrec
def co2(data: List[Vector[Char]], idx: Int = 0): Vector[Char] =
  if (data.length == 1) data.head
  else
    data.partition(_(idx) == '1') match {
      case (ones, zeros) if ones.length == zeros.length => co2(zeros, idx + 1)
      case (ones, zeros) if ones.length > zeros.length  => co2(zeros, idx + 1)
      case (ones, _)                                    => co2(ones, idx + 1)
    }

oxygen(data).asDecimal * co2(data).asDecimal
