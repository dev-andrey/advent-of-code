val input = scala.io.Source.fromResource(s"advent2023/day19.txt").toSeq.mkString

val split = input.split("\n\n")

val workflows = split.head
  .split('\n')
  .map { case s"${label}{${flows}}" =>
    label -> flows.split(',').toSeq
  }
  .toMap

case class Range(min: Int, max: Int):
  def splitLessThan(rating: Int)    = (Range(min, rating - 1), Range(rating, max))
  def splitGreaterThan(rating: Int) = (Range(min, rating), Range(rating + 1, max))

case class Part(xmas: Map[String, Range], nextFlow: String):
  def splitBy(rule: String): Seq[Part] =
    rule match
      case s"${category}<${rating}:${next}" =>
        xmas(category) match
          case Range(min, max) if max < rating.toInt => Seq(this.copy(nextFlow = next))
          case Range(min, max) if min > rating.toInt => Seq(this)
          case range                                 =>
            val (lt, gt) = range.splitLessThan(rating.toInt)
            Seq(
              this.copy(xmas = xmas.updated(category, lt), nextFlow = next),
              this.copy(xmas = xmas.updated(category, gt))
            )

      case s"${category}>${rating}:${next}" =>
        xmas(category) match
          case Range(min, max) if min > rating.toInt => Seq(this.copy(nextFlow = next))
          case Range(min, max) if max < rating.toInt => Seq(this)
          case range                                 =>
            val (lt, gt) = range.splitGreaterThan(rating.toInt)
            Seq(
              this.copy(xmas = xmas.updated(category, lt)),
              this.copy(xmas = xmas.updated(category, gt), nextFlow = next)
            )

      case next => Seq(this.copy(nextFlow = next))

@scala.annotation.tailrec
def checkWorkflow(partRanges: Seq[Part], label: String, rules: Seq[String]): Seq[Part] =
  val (current, others) = partRanges.partition(_.nextFlow == label)
  if current.isEmpty || rules.isEmpty then current ++ others
  else
    val rule      = rules.head
    val newRanges = current.flatMap(_.splitBy(rule))
    checkWorkflow(others ++ newRanges.filterNot(_.nextFlow == "R"), label, rules.tail)

@scala.annotation.tailrec
def sortPartRange(partRanges: Seq[Part], accepted: Seq[Part]): Seq[Part] =
  if partRanges.isEmpty then accepted
  else
    val partRange                    = partRanges.head
    val (justAccepted, needMoreSort) =
      checkWorkflow(Seq(partRange), partRange.nextFlow, workflows(partRange.nextFlow)).partition(_.nextFlow == "A")
    sortPartRange(needMoreSort ++ partRanges.tail, accepted ++ justAccepted)

// part1 inputs:
val parts = split.last
  .split('\n')
  .map { case s"{x=${x},m=${m},a=${a},s=${s}}" =>
    Part(
      Map(
        "x" -> Range(x.toInt, x.toInt),
        "m" -> Range(m.toInt, m.toInt),
        "a" -> Range(a.toInt, a.toInt),
        "s" -> Range(s.toInt, s.toInt)
      ),
      "in"
    )
  }
  .toSeq

val part1 = sortPartRange(parts, Seq.empty)
  .map(_.xmas.values.map(_.min).sum)
  .sum

// part 2 ranges:
val partsWithRanges = Seq(
  Part(
    Map(
      "x" -> Range(1, 4000),
      "m" -> Range(1, 4000),
      "a" -> Range(1, 4000),
      "s" -> Range(1, 4000)
    ),
    "in"
  )
)

val part2 = sortPartRange(partsWithRanges, Seq.empty)
  .map(_.xmas.values.map(range => range.max - range.min + 1L).product)
  .sum
