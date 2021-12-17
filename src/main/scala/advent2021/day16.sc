import java.math.BigInteger

val raw = scala.io.Source.fromResource(s"advent2021/day16.txt").getLines().toList.head
val hexMap = Map(
  '0' -> "0000",
  '1' -> "0001",
  '2' -> "0010",
  '3' -> "0011",
  '4' -> "0100",
  '5' -> "0101",
  '6' -> "0110",
  '7' -> "0111",
  '8' -> "1000",
  '9' -> "1001",
  'A' -> "1010",
  'B' -> "1011",
  'C' -> "1100",
  'D' -> "1101",
  'E' -> "1110",
  'F' -> "1111"
)

val input = raw.map(hexMap).mkString

implicit class StringOps(str: String) { self =>
  def bitToInt: Int = new BigInteger(str, 2).intValue()

  def bitToLong: Long = new BigInteger(str, 2).longValue()
}

sealed trait Packet {
  def ver: Int
  def id: Int
}
object Packet {
  final case class Num(ver: Int, id: Int, num: Long)              extends Packet
  final case class Op(ver: Int, id: Int, packets: Vector[Packet]) extends Packet
}

def decodeNum(idx: Int, bits: String = ""): (String, Int) =
  if (input(idx) == '0') (bits + input.slice(idx + 1, idx + 5), idx + 5)
  else decodeNum(idx + 5, bits + input.slice(idx + 1, idx + 5))

def decode(idx: Int, lastIdx: Int, packets: Vector[Packet], maxCount: Int): (Vector[Packet], Int) =
  if (packets.length == maxCount || lastIdx - idx <= 6) (packets, idx)
  else {
    val ver = input.slice(idx, idx + 3).bitToInt
    val id  = input.slice(idx + 3, idx + 6).bitToInt

    if (id == 4) {
      val (bits, newIdx) = decodeNum(idx + 6)
      decode(newIdx, lastIdx, packets :+ Packet.Num(ver, id, bits.bitToLong), maxCount)
    } else if (input(idx + 6).asDigit == 0) {
      val len             = input.slice(idx + 7, idx + 7 + 15).bitToInt
      val (subPackets, _) = decode(idx + 7 + 15, idx + 7 + 15 + len, Vector.empty, maxCount)
      decode(idx + 7 + 15 + len, lastIdx, packets :+ Packet.Op(ver, id, subPackets), maxCount)
    } else {
      val count                = input.slice(idx + 7, idx + 7 + 11).bitToInt
      val (subPackets, newIdx) = decode(idx + 7 + 11, lastIdx, Vector.empty, count)
      decode(newIdx, lastIdx, packets :+ Packet.Op(ver, id, subPackets), maxCount)
    }
  }

val (packets, _) = decode(0, input.length - 1, Vector(), Int.MaxValue)


def sumVersions(packet: Packet): Int =
  packet match {
    case Packet.Num(ver, _, _)      => ver
    case Packet.Op(ver, _, packets) => packets.map(sumVersions).sum + ver
  }

def versions(packet: Packet): Vector[Int] =
  packet match {
    case Packet.Num(ver, id, num)    => Vector(ver)
    case Packet.Op(ver, id, packets) => ver +: packets.flatMap(versions)
  }

versions(packets.head).sum
packets.map(sumVersions).sum

def doOps(packet: Packet): Long =
  packet match {
    case Packet.Op(_, 0, packets) => packets.map(doOps).sum
    case Packet.Op(_, 1, packets) => packets.map(doOps).product
    case Packet.Op(_, 2, packets) => packets.map(doOps).min
    case Packet.Op(_, 3, packets) => packets.map(doOps).max
    case Packet.Num(_, 4, num)    => num
    case Packet.Op(_, 5, packets) =>
      val pair = packets.map(doOps)
      if (pair.head > pair.last) 1L else 0L
    case Packet.Op(_, 6, packets) =>
      val pair = packets.map(doOps)
      if (pair.head < pair.last) 1L else 0L
    case Packet.Op(_, 7, packets) =>
      val pair = packets.map(doOps)
      if (pair.head == pair.last) 1L else 0L
    case _ => 0L
  }

packets.map(doOps).head
