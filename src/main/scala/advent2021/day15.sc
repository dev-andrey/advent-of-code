val raw = scala.io.Source.fromResource(s"advent2021/day15.txt").getLines()

val (maxRow, maxCol, input) = raw.foldLeft((0, 0, Map.empty[(Int, Int), Int])) { case (row, _, grid) -> line =>
  (row + 1, line.length, grid ++ line.zipWithIndex.map(_.swap).map { case (x, ch) => (x, row) -> ch.asDigit })
}

final case class Loc(x: Int, y: Int)

val baseStart = Loc(0, 0)
final case class GameMap(end: Loc, risks: Map[(Int, Int), Int]) {
  def maxX: Int = end.x

  def maxY: Int = end.y

  def adjacent(loc: Loc): Vector[Loc] = loc match {
    case Loc(x, y) =>
      Vector(Loc(x - 1, y), Loc(x + 1, y), Loc(x, y - 1), Loc(x, y + 1))
        .filter(loc => loc.x >= 0 && loc.x <= maxX && loc.y >= 0 && loc.y <= maxY)
  }

  def risk(loc: Loc): Int = {
    val base   = risks((loc.x % maxCol, loc.y % maxRow))
    val adjust = loc.x / maxCol + loc.y / maxRow

    if (base + adjust > 9) (base + adjust) % 9 else base + adjust

  }
}

//@scala.annotation.tailrec
//def find(analyzed: Set[Loc], riskTable: Map[Loc, Int])(gameMap: GameMap): Int = {
//  val candidate = riskTable.filterNot { case (loc, _) => analyzed(loc) }.minByOption(_._2)
//  if (candidate.isEmpty) riskTable(gameMap.end)
//  else {
//    val (curr, risk) = candidate.get
//    val updatedTable = gameMap
//      .adjacent(curr)
//      .filterNot(analyzed)
//      .filterNot(loc => riskTable.get(loc).exists(_ < risk + gameMap.risk(loc)))
//      .foldLeft(riskTable) { case (newMap, loc) =>
//        newMap + (loc -> (risk + gameMap.risk(loc)))
//      }
//
//    find(analyzed + curr, updatedTable)(gameMap)
//  }
//}

@scala.annotation.tailrec
def find(analyzed: Set[Loc], riskVector: Vector[(Loc, Int)])(gameMap: GameMap): Int = {
  val (curr, risk) = riskVector.head
  if (curr == gameMap.end) risk
  else {
    val updatedRisks = gameMap
      .adjacent(curr)
      .filterNot(analyzed)
      .map(loc => loc -> (gameMap.risks(loc.x, loc.y) + risk))
      .toMap
    println(updatedRisks)

    val updatedVector = riskVector.tail
      .map { case (loc, rsk) =>
        if (updatedRisks(loc) < rsk)
          (loc, updatedRisks(loc))
        else (loc, rsk)
      }
      .sortBy(-_._2)

    println(updatedVector)

    find(analyzed + curr, updatedVector)(gameMap)
  }
}

find(Set(), Vector(baseStart -> 0))(GameMap(Loc(maxCol - 1, maxRow - 1), input))

//find(Set(), Map(baseStart -> 0))(GameMap(Loc(maxCol * 5 - 1, maxRow * 5 - 1), input))

//@tailrec
//def find(analyzed: Set[Loc], riskTable: mutable.PriorityQueue[(Loc, Int)])(gameMap: GameMap): Int = {
//  val (curr, risk) = riskTable.dequeue()
//  if (curr == gameMap.end) risk
//  else {
//    find(
//      analyzed + curr,
//      riskTable.addAll(
//        gameMap
//          .adjacent(curr)
//          .filterNot(analyzed)
//          .map(loc => (loc, gameMap.risk(loc) + risk))
//      )
//    )(gameMap)
//  }
//}
//val initialQueue = mutable.PriorityQueue.from(Iterable((baseStart, 0))) { (x: (Loc, Int), y: (Loc, Int)) =>
//  y._2 - x._2
//}
//
//find(Set(), initialQueue)(GameMap(Loc(maxCol * 5 - 1, maxRow * 5 - 1), input))

//find(Set(), Map(baseStart -> 0))(GameMap(Loc(maxCol - 1, maxRow - 1), input))

//find(Set(), Map(baseStart -> 0))(GameMap(Loc(maxCol * 5 - 1, maxRow * 5 - 1), input))
