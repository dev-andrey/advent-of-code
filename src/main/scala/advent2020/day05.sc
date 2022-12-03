val input = scala.io.Source.fromResource(s"advent2020/day05.txt").getLines().toList

input
  .map(ticketCode => (ticketCode.take(7), ticketCode.takeRight(3)))
  .map { case (rowCode, seatCode) =>
    def decode(code: String, maxNum: Int): Int = code
      .foldLeft((0, maxNum)) {
        case ((min, max), 'F' | 'L') => (min, min + (max - min) / 2)
        case ((min, max), 'B' | 'R') => (min + (max - min) / 2 + 1, max)
      }
      ._1

    decode(rowCode, 127) * 8 + decode(seatCode, 7)
  }
  // .max
  .sorted
  .sliding(2)
  .find(_.reduce(_ - _).abs > 1)
  .map(_.head + 1)
