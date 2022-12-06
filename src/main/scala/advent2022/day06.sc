import scala.annotation.tailrec

val input = scala.io.Source.fromResource(s"advent2022/day06.txt").getLines().toSeq

@tailrec
def findMarker(dataStream: String)(idx: Int, buffer: Vector[Char], charCount: Int): Int =
  if (buffer.distinct.length == charCount) idx
  else if (buffer.length < charCount) findMarker(dataStream)(idx + 1, dataStream(idx) +: buffer, charCount)
  else findMarker(dataStream)(idx + 1, dataStream(idx) +: buffer.dropRight(1), charCount)

// part 1
input
  .map(dataStream => findMarker(dataStream)(0, Vector.empty, 4))

// part 2
input
  .map(dataStream => findMarker(dataStream)(0, Vector.empty, 14))
