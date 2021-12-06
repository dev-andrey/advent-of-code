val input = scala.io.Source.fromResource(s"advent2021/day06.txt").getLines().toVector

val state = input.flatMap(_.split(',')).map(_.toInt).groupMapReduce(identity)(_ => 1L)(_ + _)

@scala.annotation.tailrec
def cycle(remainingCycles: Int, currDay: Map[Int, Long]): Map[Int, Long] =
  if (remainingCycles == 0) currDay
  else
    cycle(
      remainingCycles - 1,
      currDay.foldLeft(Map.empty[Int, Long]) {
        case (nextDay, (day, fishCount)) if day == 0 =>
          nextDay
            .updated(6, nextDay.getOrElse(6, 0L) + fishCount)
            .updated(8, nextDay.getOrElse(8, 0L) + fishCount)
        case (result, (day, fishCount)) =>
          result
            .updated(day - 1, result.getOrElse(day - 1, 0L) + fishCount)
      }
    )

cycle(80, state).values.sum

cycle(256, state).values.sum
