val lines = scala.io.Source.fromResource(s"advent2023/day04.txt").getLines().toList

val cards = lines
  .map { case s"Card ${num}: ${winningNumbers} | ${numbersOnTheCard}" =>
    val winningSet  = winningNumbers.trim.split("\\s+").map(_.toInt).toSet
    val cardNumbers = numbersOnTheCard.trim.split("\\s+").map(_.toInt)
    cardNumbers.count(winningSet.contains)
  }

val part1 = cards
  .filter(_ > 0)
  .map(cnt => Math.pow(2, cnt - 1).toInt)
  .sum

val (_, part2) =
  cards
    .foldLeft((List.fill(cards.length)(1), 0)) {
      case (cnt :: cardCnt, total) -> winCnt =>
        (cardCnt.take(winCnt).map(_ + cnt) ++ cardCnt.drop(winCnt), total + cnt)

      case x -> _ => x // impossible as both collections are reducing simultaneously
    }

// alternative part 2 using tail recursion
@scala.annotation.tailrec
def totalCards(remain: List[(Int, Int)], total: Int): Int =
  remain match
    case (winCnt, cnt) :: next =>
      totalCards(
        next.take(winCnt).map(x => x.copy(_2 = x._2 + cnt)) ++ next.drop(winCnt),
        total + cnt
      )

    case Nil => total

val part2alt = totalCards(cards.map(_ -> 1), 0)
