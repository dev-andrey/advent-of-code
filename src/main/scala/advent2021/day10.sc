val raw = scala.io.Source.fromResource(s"advent2021/day10.txt").getLines()

val input = raw.toList
val score = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
val valid = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

@scala.annotation.tailrec
def analyze(line: List[Char], opened: List[Char]): Either[List[Char], List[Char]] =
  line match {
    case Nil                                                    => Right(opened)
    case open :: tail if Set('<', '[', '(', '{').contains(open) => analyze(tail, open :: opened)
    case close :: _ if opened.head != valid(close)              => Left(List(close))
    case _ :: tail                                              => analyze(tail, opened.tail)
  }

// part 1
input
  .flatMap(line =>
    analyze(line.toList, Nil) match {
      case Left(corrupted) => corrupted
      case Right(_)        => List()
    }
  )
  .map(score)
  .sum

// part 2
val autoComplete = valid.map { case (close, open) => (open, close) }
val autoScore    = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

val scores = input
  .flatMap(line =>
    analyze(line.toList, Nil) match {
      case Left(_) => List()
      case Right(incomplete) =>
        List(incomplete.map(autoComplete).foldLeft(0L) { case score -> ch =>
          score * 5L + autoScore(ch)
        })
    }
  )
  .sorted

scores.drop(scores.length / 2).head
