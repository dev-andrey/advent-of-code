val input = scala.io.Source.fromResource(s"advent2020/day09.txt").getLines().map(_.toLong).toVector

@annotation.tailrec
def findWrong(data: Vector[Long], preamble: Vector[Long]): Long =
  if (preamble.exists(pre => preamble.contains(data.head - pre))) findWrong(data.tail, preamble.tail :+ data.head)
  else data.head

val res0 = findWrong(input.drop(25), input.take(25))

@annotation.tailrec
def findContiguous(lo: Int, hi: Int, target: Long): Long =
  input.slice(lo, hi).sum match {
    case sum if sum == target => input.slice(lo, hi).min + input.slice(lo, hi).max
    case sum if sum > target  => findContiguous(lo + 1, hi, target)
    case sum if sum < target  => findContiguous(lo, hi + 1, target)
  }

findContiguous(0, 1, res0)
