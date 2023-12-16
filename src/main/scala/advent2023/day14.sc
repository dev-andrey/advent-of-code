val platform = scala.io.Source.fromResource(s"advent2023/day14.txt").getLines().toSeq

def calcLoad(lines: Seq[String]) =
  lines.reverse.zipWithIndex.map { case row -> idx =>
    row.count(_ == 'O') * (idx + 1)
  }.sum

@scala.annotation.tailrec
def tiltNorth(lines: Seq[String]): Seq[String] =
  val rolled = lines.tail.foldLeft(Seq(lines.head)) { case moved -> curr =>
    val swapped = (moved.last zip curr).map {
      case ('.', 'O') => ('O', '.')
      case (p, c)     => (p, c)
    }
    val last    = swapped.map(_._1).mkString
    val updated = swapped.map(_._2).mkString
    moved.init :+ last :+ updated
  }
  if rolled == lines then rolled
  else tiltNorth(rolled)

val part1 = calcLoad(tiltNorth(platform))

// Part 2

def rotateClockwise(seq: Seq[String])        = seq.map(_.toSeq).transpose.map(_.reverse.mkString)
def rotateCounterClockwise(seq: Seq[String]) = seq.map(_.toSeq).transpose.reverse.map(_.mkString)

def tiltSouth(lines: Seq[String]) = tiltNorth(lines.reverse).reverse
def tiltEast(lines: Seq[String])  = rotateClockwise(tiltNorth(rotateCounterClockwise(lines)))
def tiltWest(lines: Seq[String])  = rotateCounterClockwise(tiltNorth(rotateClockwise(lines)))

def findCycle(sequence: Seq[Int], minCycleLength: Int): Option[(Int, Int)] = {
  for
    cycleLength <- minCycleLength to sequence.length / 2
    startIndex  <- (0 to sequence.length - 2 * cycleLength).view
    if (startIndex until startIndex + cycleLength)
      .lazyZip(startIndex + cycleLength until startIndex + 2 * cycleLength)
      .forall { case (i, j) => sequence(i) == sequence(j) }
  yield (startIndex, startIndex + cycleLength - 1)
}.headOption

@scala.annotation.tailrec
def spinCycle(times: Long, lines: Seq[String], load: Seq[Int]): Seq[String] =
  findCycle(load, minCycleLength = 3) match
    case Some((start, end)) =>
      spinCycle(times % (end - start + 1), lines, Seq())

    case None =>
      if times == 0 then lines
      else
        val tilted     = tiltEast(tiltSouth(tiltWest(tiltNorth(lines))))
        val tiltedLoad = calcLoad(tilted)
        spinCycle(times - 1, tilted, load :+ tiltedLoad)

val part2 = calcLoad(spinCycle(1_000_000_000L, platform, Seq.empty))
