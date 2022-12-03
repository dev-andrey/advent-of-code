import scala.annotation.tailrec
import scala.collection.mutable

final case class Loc(x: Int, y: Int)

val hallway = (0 to 10).filter(x => x != 2 && x != 4 && x != 6 && x != 8).map(x => Loc(x, 0)).toSet

final case class Pod(loc: Loc, price: Int) { self =>
  def dest: Int =
    if (price == 1) 2
    else if (price == 10) 4
    else if (price == 100) 6
    else 8

  def hasRightRoom = loc.x == dest

  def hasWrongRoom = !hasRightRoom

  def inRoom: Boolean = loc.y < 0

  def inHall: Boolean = !inRoom

}
val destMap = Map(2 -> 'A', 4 -> 'B', 6 -> 'C', 8 -> 'D')

final case class Game(pods: Set[Pod], total: Int, depth: Int) { self =>
  def hasAllInPlace: Boolean =
    pods.forall(pod => pod.loc.x == pod.dest)

  def nextMoves: List[Game] =
    for {
      pod   <- self.pods.toList
      moved <- moves(pod, self.pods - pod)
      cost = moved.price * ((pod.loc.x - moved.loc.x).abs + (pod.loc.y - moved.loc.y).abs)
    } yield self.copy((self.pods - pod) + moved, total + cost)

  def moves(pod: Pod, others: Set[Pod]): Set[Pod] =
    if (pod.inRoom) {
      val blockedByOthers =
        others.exists(other => other.loc.x == pod.loc.x && other.loc.y > pod.loc.y)
      val blockingOthers =
        others.exists(other => other.loc.x == pod.loc.x && other.loc.y < pod.loc.y && other.hasWrongRoom)

      val noNeedToMove = pod.hasRightRoom && !blockingOthers

      if (blockedByOthers || noNeedToMove) Set()
      else {
        val leftWall =
          others.filter(other => other.inHall && other.loc.x < pod.loc.x).map(_.loc.x).maxOption.getOrElse(-1)
        val rightWall =
          others.filter(other => other.inHall && other.loc.x > pod.loc.x).map(_.loc.x).minOption.getOrElse(11)

        hallway
          .filter(loc => loc.x > leftWall && loc.x < rightWall)
          .map(loc => pod.copy(loc = loc))
      }
    } else {
      val destRoomPods = others.filter(_.loc.x == pod.dest)

      val destRoomOccupied = destRoomPods.exists(other => other.dest != pod.dest)

      val pathIsBlocked = others
        .filter(_.inHall)
        .map(_.loc.x)
        .exists(x => x > pod.dest && x < pod.loc.x || x < pod.dest && x > pod.loc.x)

      if (destRoomOccupied || pathIsBlocked) Set()
      else
        (-depth to -1)
          .map(y => Loc(pod.dest, y))
          .filterNot(loc => destRoomPods.exists(_.loc == loc))
          .map(loc => pod.copy(loc = loc))
          .minByOption(_.loc.y)
          .toSet
    }

  def pretty(): String = {
    def pod(x: Int, y: Int): Char =
      pods.find(_.loc == Loc(x, y)).map(pod => destMap(pod.dest)).getOrElse('.')

    val hallway = (0 to 10).map(x => pod(x, 0)).mkString

    s"""
       |$total
       |#############
       |#$hallway#
       |###${pod(2, -1)}#${pod(4, -1)}#${pod(6, -1)}#${pod(8, -1)}###
       |  #${pod(2, -2)}#${pod(4, -2)}#${pod(6, -2)}#${pod(8, -2)}#
       |  #${pod(2, -3)}#${pod(4, -3)}#${pod(6, -3)}#${pod(8, -3)}#
       |  #${pod(2, -4)}#${pod(4, -4)}#${pod(6, -4)}#${pod(8, -4)}#
       |  #########
       |""".stripMargin
  }

}

@tailrec
def simulate(games: mutable.PriorityQueue[Game], seen: Set[Set[Pod]], minCost: Int): Int =
  if (games.isEmpty || games.head.total > minCost) minCost
  else {
    val head = games.dequeue()

    if (seen.contains(head.pods)) simulate(games, seen, minCost)
    else if (head.hasAllInPlace) simulate(games, seen, minCost min head.total)
    else {
      val nextMoves = head.nextMoves.filterNot(game => seen(game.pods))

      if (nextMoves.isEmpty) simulate(games, seen + head.pods, minCost)
      else simulate(games.addAll(nextMoves), seen + head.pods, minCost)
    }
  }

val part1 = Game(
  Set(
    Pod(Loc(2, -1), 1000),
    Pod(Loc(2, -2), 100),
    Pod(Loc(4, -1), 1),
    Pod(Loc(4, -2), 1),
    Pod(Loc(6, -1), 100),
    Pod(Loc(6, -2), 10),
    Pod(Loc(8, -1), 1000),
    Pod(Loc(8, -2), 10)
  ),
  total = 0,
  depth = 2
)

val part2 = Game(
  Set(
    Pod(Loc(2, -1), 1000),
    Pod(Loc(2, -2), 1000),
    Pod(Loc(2, -3), 1000),
    Pod(Loc(2, -4), 100),
    Pod(Loc(4, -1), 1),
    Pod(Loc(4, -2), 100),
    Pod(Loc(4, -3), 10),
    Pod(Loc(4, -4), 1),
    Pod(Loc(6, -1), 100),
    Pod(Loc(6, -2), 10),
    Pod(Loc(6, -3), 1),
    Pod(Loc(6, -4), 10),
    Pod(Loc(8, -1), 1000),
    Pod(Loc(8, -2), 1),
    Pod(Loc(8, -3), 100),
    Pod(Loc(8, -4), 10)
  ),
  total = 0,
  depth = 4
)

simulate(mutable.PriorityQueue.from(List(part2))((g1, g2) => g2.total - g1.total), Set(), Int.MaxValue)

/*
#############
#...........#
###D#A#C#D###
  #C#A#B#B#
  #########
 */
