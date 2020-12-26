val input = scala.io.Source.fromResource(s"advent2020/day22.txt").getLines().toVector

val (p1Input, p2Input) = input.filterNot(_.startsWith("Player")).span(_.nonEmpty)
val p1deck             = p1Input.map(_.toInt).toList
val p2deck             = p2Input.filter(_.nonEmpty).map(_.toInt).toList

@annotation.tailrec
def play(deck1: List[Int], deck2: List[Int]): List[Int] =
  (deck1, deck2) match {
    case (p1won, Nil) => p1won
    case (Nil, p2won) => p2won

    case (top1 :: remain1, top2 :: remain2) =>
      if (top1 > top2) play(remain1 ++ List(top1, top2), remain2)
      else play(remain1, remain2 ++ List(top2, top1))
  }

val score = play(p1deck, p2deck)

score.reverse.zipWithIndex.map { case (card, idx) => card * (idx + 1) }.sum

def recursiveCombat(
  deck1: List[Int],
  deck2: List[Int],
  played: Set[(List[Int], List[Int])]
): (List[Int], List[Int]) =
  if (played.contains((deck1, deck2))) (deck1, Nil)
  else
    (deck1, deck2) match {
      case (p1won, Nil) => (p1won, Nil)
      case (Nil, p2won) => (Nil, p2won)

      case (top1 :: remain1, top2 :: remain2) if top1 <= remain1.length && top2 <= remain2.length =>
        recursiveCombat(remain1.take(top1), remain2.take(top2), Set.empty[(List[Int], List[Int])]) match {
          case (_, Nil) => recursiveCombat(remain1 ++ List(top1, top2), remain2, played + ((deck1, deck2)))
          case (Nil, _) => recursiveCombat(remain1, remain2 ++ List(top2, top1), played + ((deck1, deck2)))
        }

      case (top1 :: remain1, top2 :: remain2) =>
        if (top1 > top2) recursiveCombat(remain1 ++ List(top1, top2), remain2, played + ((deck1, deck2)))
        else recursiveCombat(remain1, remain2 ++ List(top2, top1), played + ((deck1, deck2)))

    }

val score2 = recursiveCombat(p1deck, p2deck, Set.empty[(List[Int], List[Int])])
score2._1.reverse.zipWithIndex.map { case (card, idx) => card * (idx + 1) }.sum
score2._2.reverse.zipWithIndex.map { case (card, idx) => card * (idx + 1) }.sum
