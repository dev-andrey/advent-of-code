val input = scala.io.Source.fromResource(s"advent2022/day13.txt").getLines().toSeq

sealed trait Packet { self =>
  def compareTo(other: Packet): Int =
    (self, other) match {
      case (Packet.Single(l), Packet.Single(r))              => l compareTo r
      case (left @ Packet.Coll(_), right @ Packet.Single(_)) => left compareTo Packet.Coll(Vector(right))
      case (left @ Packet.Single(_), right @ Packet.Coll(_)) => Packet.Coll(Vector(left)) compareTo right
      case (Packet.Coll(l), Packet.Coll(r)) =>
        def compare(left: Vector[Packet], right: Vector[Packet]): Int =
          if (left.isEmpty && right.isEmpty) 0
          else if (left.isEmpty) -1
          else if (right.isEmpty) 1
          else if ((left.head compareTo right.head) == 0) compare(left.tail, right.tail)
          else left.head compareTo right.head

        compare(l, r)
    }
}
object Packet {
  final case class Single(int: Int)              extends Packet
  final case class Coll(packets: Vector[Packet]) extends Packet
}

def parseString(packets: String): Packet = {
  def parse(coll: String, l: Int, r: Int, opened: Int, acc: Vector[Packet]): Vector[Packet] =
    if (r == coll.length) acc :+ parseString(coll.substring(l, r))
    else {
      coll(r) match {
        case '['                => parse(coll, l, r + 1, opened + 1, acc)
        case ']'                => parse(coll, l, r + 1, opened - 1, acc)
        case ',' if opened == 0 => parse(coll, r + 1, r + 1, opened, acc :+ parseString(coll.substring(l, r)))
        case _                  => parse(coll, l, r + 1, opened, acc)
      }
    }

  packets match {
    case "[]"             => Packet.Coll(Vector.empty)
    case s"[$collection]" => Packet.Coll(parse(collection, 0, 0, 0, Vector.empty))
    case single           => Packet.Single(single.toInt)
  }
}

def compare(left: String, right: String): Boolean =
  (parseString(left) compareTo parseString(right)) == -1

// part 1
input
  .filter(_.nonEmpty)
  .grouped(2)
  .zipWithIndex
  .collect {
    case (pair, idx) if compare(pair.head, pair.last) => idx + 1
  }
  .sum

// part 2
val dividers = Seq("[[2]]", "[[6]]")
val sorted   = (input.filter(_.nonEmpty) ++ dividers).sortWith(compare)
dividers.map(div => sorted.indexOf(div) + 1).product
