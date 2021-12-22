import scala.util.chaining.scalaUtilChainingOps

final case class Player(pos: Int, score: Int) { self =>
  def move(num: Int): Player =
    ((pos + num) % 10) pipe
      (position => if (position == 0) 10 else position) pipe
      (position => self.copy(pos = position, score = score + position))
}

final case class Dice(curr: Vector[Int], rolled: Int) {
  def next = Dice(
    (curr.last + 1 to curr.last + curr.length).map(d => if (d > 100) d % 100 else d).toVector,
    rolled + curr.length
  )
}

@scala.annotation.tailrec
def game(turn: Int, dice: Dice, player1: Player, player2: Player): Long =
  if (player1.score >= 1000 || player2.score >= 1000)
    (player1.score min player2.score) * dice.rolled
  else
    game(
      turn + 1,
      dice.next,
      if (turn % 2 == 0) player1.move(dice.curr.sum) else player1,
      if (turn % 2 != 0) player2.move(dice.curr.sum) else player2
    )

// part 1
game(0, Dice(Vector(1, 2, 3), 0), Player(8, 0), Player(3, 0))

def memo[K, V](f: K => V): K => V = {
  val cache = scala.collection.mutable.Map.empty[K, V]
  k => cache.getOrElseUpdate(k, f(k))
}

def diracRoll = for {
  roll1 <- 1 to 3
  roll2 <- 1 to 3
  roll3 <- 1 to 3
} yield roll1 + roll2 + roll3

type Game     = (Player, Player)
type WinCount = (Long, Long)

lazy val dirac: Game => WinCount = memo { case (p1, p2) =>
  if (p1.score >= 21) (1L, 0L)
  else if (p2.score >= 21) (0L, 1L)
  else
    diracRoll
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toVector
      .map { case (rollSum, rollCount) =>
        val (p2Wins, p1Wins) = dirac(p2, p1.move(rollSum))
        (rollCount * p1Wins, rollCount * p2Wins)
      }
      .foldLeft((0L, 0L)) { case ((p1Total, p2Total), (p1Wins, p2Wins)) =>
        (p1Total + p1Wins, p2Total + p2Wins)
      }
}

// part 2
dirac(Player(8, 0), Player(3, 0)) pipe { case (p1, p2) => p1 max p2 }
