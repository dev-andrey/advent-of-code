val input = scala.io.Source.fromResource(s"advent2020/day14.txt").getLines().toList

val instr = "mem\\[(\\d+)] = (\\d+)".r

def decodeValue(binary: String, mask: String): Long =
  (("0" * (mask.length - binary.length) + binary) zip mask)
    .foldLeft(0L) {
      case (out, ('1', 'X') | (_, '1')) => out << 1 | 1
      case (out, _)                     => out << 1
    }

input
  .foldLeft(Map.empty[Int, Long], "") {
    case (mem, _) -> line if line startsWith "mask" =>
      (mem, line.drop(7))

    case (mem, mask) -> line =>
      line match {
        case instr(addr, value) =>
          (
            mem + (addr.toInt -> decodeValue(value.toLong.toBinaryString, mask)),
            mask
          )
      }
  }
  ._1
  .values
  .sum

def decodeMemory(addr: String, mask: String): List[Long] =
  ("0" * (mask.length - addr.length) + addr zip mask)
    .foldLeft(List(0L)) {
      case (out, (_, 'X'))   => out.flatMap(n => List(n << 1, n << 1 | 1))
      case (out, (_, '1'))   => out.map(_ << 1 | 1)
      case (out, ('1', '0')) => out.map(_ << 1 | 1)
      case (out, _)          => out.map(_ << 1)
    }

input
  .foldLeft(Map.empty[Long, Long], "") {
    case (mem, _) -> line if line startsWith "mask" =>
      (mem, line.drop(7))

    case (mem, mask) -> line =>
      line match {
        case instr(addr, value) =>
          (
            mem ++ decodeMemory(addr.toLong.toBinaryString, mask).map(_ -> value.toLong),
            mask
          )
      }
  }
  ._1
  .values
  .sum
