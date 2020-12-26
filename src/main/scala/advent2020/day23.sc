val input = "198753462".map(_.toInt - '0')

val minValue = input.min
val maxValue = 1_000_000

final case class Link(value: Int, next: Int) {
  override def toString = s"$value-->$next"
}

val initMap = Map(input.head -> Link(input.head, input.tail.head))

val (oneWay, last) =
  (input.tail ++ (input.max + 1 to maxValue)).foldLeft((initMap, initMap.head._1)) { case (map, prev) -> elem =>
    (map.updated(prev, Link(prev, elem)), elem)
  }

val cycle     = oneWay.updated(last, Link(last, input.head))
val maxValues = Set(maxValue).flatMap(v => Set(v, v - 1, v - 2, v - 3))

@annotation.tailrec
def game(move: Int, cup: Int, cups: Map[Int, Link]): Long =
  if (move == 0) cups(1).next.toLong * cups(cups(1).next).next.toLong
  else {
    val curr = cups(cup)
    val n1   = cups(curr.next)
    val n2   = cups(n1.next)
    val n3   = cups(n2.next)
    val tail = cups(n3.next)

    @annotation.tailrec
    def pickDestination(target: Int): Int =
      if (target == n1.value || target == n2.value || target == n3.value) pickDestination(target - 1)
      else if (target < minValue) (maxValues diff Set(n1.value, n2.value, n3.value)).max
      else target

    val dest = pickDestination(curr.value - 1)

    val target      = cups(dest)
    val afterTarget = cups(target.next)

    val updated =
      cups
        .updated(curr.value, curr.copy(next = tail.value))
        .updated(target.value, target.copy(next = n1.value))
        .updated(n3.value, n3.copy(next = afterTarget.value))

    game(move - 1, tail.value, updated)
  }

game(10_000_000, input.head, cycle)
