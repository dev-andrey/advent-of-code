val lines = scala.io.Source.fromResource(s"advent2023/day09.txt").getLines().toSeq

val histories = lines.map(_.split("\\s+").map(_.toInt).toSeq)

def nextValue(last: Int, diffs: List[Seq[Int]]): Int =
  if diffs.isEmpty then last
  else nextValue(diffs.head.last + last, diffs.tail)

def nextHistory(history: Seq[Int], diffs: List[Seq[Int]]): Int =
  val diffSeq = history.sliding(2).map(pair => pair.last - pair.head).toSeq
  if diffSeq.forall(_ == 0) then nextValue(diffSeq.last, diffs)
  else nextHistory(diffSeq, diffSeq :: diffs)

val part1 = histories.map(histSeq => nextHistory(histSeq, List(histSeq))).sum




def prevValue(last: Int, diffs: List[Seq[Int]]): Int =
  if diffs.isEmpty then last
  else prevValue(diffs.head.head - last, diffs.tail)

def prevHistory(history: Seq[Int], diffs: List[Seq[Int]]): Int =
  val diffSeq = history.sliding(2).map(pair => pair.last - pair.head).toSeq
  if diffSeq.forall(_ == 0) then prevValue(diffSeq.last, diffs)
  else prevHistory(diffSeq, diffSeq :: diffs)

val part2 = histories.map(histSeq => prevHistory(histSeq, List(histSeq))).sum


