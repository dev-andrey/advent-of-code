import scala.annotation.tailrec

val raw = scala.io.Source.fromResource(s"advent2021/day04.txt").getLines().toList

final case class Board(board: Vector[Vector[Int]]) {
  def hasBingo =
    board.head.indices.exists(col => board.forall(_(col) >= 100)) ||
      board.exists(_.forall(_ >= 100))

  def unmarkedSum = board.map(_.filter(_ < 100).sum).sum
}

// Parsing

val nums = raw.head.split(',').map(_.toInt).toList

val boards = raw.drop(2).foldLeft(Vector.empty[Board], Vector.empty[Vector[Int]]) {
  case (boards, board) -> line if line.isEmpty =>
    (boards :+ Board(board), Vector.empty[Vector[Int]])

  case (boards, board) -> line =>
    (boards, board :+ line.split(' ').filter(_.nonEmpty).map(_.toInt).toVector)
} match {
  case (boards, board) => boards :+ Board(board)
}

def play(num: Int, board: Board): Board =
  Board(board.board.map(_.map(n => if (n == num) n + 100 else n)))

// Part 1
@tailrec
def firstWins(nums: List[Int], boards: Vector[Board], lastNumber: Int): Int =
  boards.find(_.hasBingo) match {
    case Some(board) => board.unmarkedSum * lastNumber
    case None        => firstWins(nums.tail, boards.map(play(nums.head, _)), nums.head)
  }

firstWins(nums, boards, -1)

// Part 2
@tailrec
def lastWins(nums: List[Int], boards: Vector[Board], lastNumber: Int): Int =
  if (boards.length == 1 && boards.head.hasBingo) boards.head.unmarkedSum * lastNumber
  else lastWins(nums.tail, boards.filterNot(_.hasBingo).map(play(nums.head, _)), nums.head)

lastWins(nums, boards, -1)
