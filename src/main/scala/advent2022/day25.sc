val input = scala.io.Source.fromResource(s"advent2022/day25.txt").getLines().toSeq

def snafuToDec(num: String): Long =
  num.foldLeft(0L) {
    case res -> '0' => 5 * res + 0
    case res -> '1' => 5 * res + 1
    case res -> '2' => 5 * res + 2
    case res -> '=' => 5 * res - 2
    case res -> '-' => 5 * res - 1
  }

def decToSnafu(long: Long): String =
  if (long == 0) ""
  else
    decToSnafu((long + 2) / 5) +
      (
        long % 5 match {
          case 0 => "0"
          case 1 => "1"
          case 2 => "2"
          case 3 => "="
          case 4 => "-"
        }
      )

val sum = input.map(snafuToDec).sum

decToSnafu(sum)
