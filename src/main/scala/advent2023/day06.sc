val lines = scala.io.Source
  .fromResource(s"advent2023/day06.txt")
  .getLines()
  .toSeq
  .map {
    case s"Time: ${times}"     => times.trim.split("\\s+").map(_.toLong).toSeq
    case s"Distance: ${times}" => times.trim.split("\\s+").map(_.toLong).toSeq
  }

val races = lines.head zip lines.last

def simulate(time: Long, record: Long): Long =
  (0L to time)
    .count(holdTime => (time - holdTime) * holdTime > record)

// part 1 and part 2 (with input manipulation)
races.map { case (time, record) =>
  simulate(time, record)
}.product
