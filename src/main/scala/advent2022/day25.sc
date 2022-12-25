val input = scala.io.Source.fromResource(s"advent2022/day25.txt").getLines().toSeq

def snafuToDec(num: String): Long =
  num.foldLeft(0L) {
    case res -> '0' => 5 * res + 0
    case res -> '1' => 5 * res + 1
    case res -> '2' => 5 * res + 2
    case res -> '=' => 5 * res - 2
    case res -> '-' => 5 * res - 1
  }

@annotation.tailrec
def decToSnafu(dec: Long, result: String = ""): String =
  if (dec == 0) result
  else
    dec % 5 match {
      case 0 => decToSnafu((dec - 0) / 5, "0" + result)
      case 1 => decToSnafu((dec - 1) / 5, "1" + result)
      case 2 => decToSnafu((dec - 2) / 5, "2" + result)
      case 3 => decToSnafu((dec + 2) / 5, "=" + result)
      case 4 => decToSnafu((dec + 1) / 5, "-" + result)
    }

val sum = input.map(snafuToDec).sum

decToSnafu(sum)
