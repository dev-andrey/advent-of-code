val input = scala.io.Source.fromResource(s"advent2022/day20.txt").getLines().map(_.toLong).toList

def decrypt(idx: Int, queue: Vector[(Long, Int)]): Vector[(Long, Int)] =
  if (idx == input.length) queue
  else if (queue.head._2 != idx) {
    val (tail, head) = queue.span(_._2 != idx)
    decrypt(idx, head ++ tail)
  } else {
    val element = queue.head
    val tail    = queue.tail

    val elemsToPop = Math.floorMod(element._1, tail.size)
    val updated    = tail.drop(elemsToPop) ++ tail.take(elemsToPop) :+ element

    decrypt(idx + 1, updated)
  }

// part 1
val part1     = decrypt(0, Vector.from(input.zipWithIndex)).map(_._1)
val part1Zero = part1.indexOf(0)

Seq(1000, 2000, 3000).map(i => part1(Math.floorMod(part1Zero + i, input.length))).sum

// part 2
val part2 = (0 until 10)
  .foldLeft(Vector.from(input.map(_ * 811589153L).zipWithIndex)) { case mixed -> _ =>
    decrypt(0, mixed)
  }
  .map(_._1)
val part2Zero = part2.indexOf(0)

Seq(1000, 2000, 3000).map(i => part2(Math.floorMod(part2Zero + i, input.length))).sum
