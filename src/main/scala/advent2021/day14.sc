val raw = scala.io.Source.fromResource(s"advent2021/day14.txt").getLines()

val (template, rules) = raw.filter(_.nonEmpty).foldLeft("", Vector.empty[(String, String)]) {
  case template -> rules -> s"${pair} -> ${elem}" => (template, rules :+ (pair -> elem))
  case _ -> rules -> template                     => (template, rules)
}

val ruleMap = rules.toMap
val initial = template.sliding(2).toVector.groupMapReduce(identity)(_ => 1L)(_ + _)

@scala.annotation.tailrec
def cycle(step: Int, resultMap: Map[String, Long]): Long =
  if (step == 0) {
    val solutionGrouped = resultMap.groupMapReduce(_._1.head)(_._2)(_ + _).map {
      case ch -> freq if ch == template.last => ch -> (freq + 1L)
      case ch -> freq                        => ch -> freq
    }
    solutionGrouped.maxBy(_._2)._2 - solutionGrouped.minBy(_._2)._2
  } else {
    cycle(
      step - 1,
      resultMap.foldLeft(Map.empty[String, Long]) { case (freqMap, (pair, freq)) =>
        val inserted = ruleMap(pair).flatMap(ch => Vector(s"${pair.head}$ch", s"$ch${pair.last}"))
        freqMap
          .updated(inserted.head, freqMap.getOrElse(inserted.head, 0L) + freq)
          .updated(inserted.last, freqMap.getOrElse(inserted.last, 0L) + freq)
      }
    )
  }

// part 1
cycle(10, initial)

// part 2
cycle(40, initial)
