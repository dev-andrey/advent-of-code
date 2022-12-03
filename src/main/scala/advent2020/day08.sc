val input = scala.io.Source.fromResource(s"advent2020/day08.txt").getLines().toArray

val instr = "(nop|acc|jmp) ([+\\-])(\\d+)".r

@annotation.tailrec
def execute(idx: Int = 0, seen: Set[Int] = Set(), acc: Int = 0): Int =
  if (seen.contains(idx)) acc
  else
    input(idx) match {
      case instr("nop", _, _)        => execute(idx + 1, seen + idx, acc)
      case instr("jmp", "+", offset) => execute(idx + offset.toInt, seen + idx, acc)
      case instr("jmp", "-", offset) => execute(idx - offset.toInt, seen + idx, acc)
      case instr("acc", "+", offset) => execute(idx + 1, seen + idx, acc + offset.toInt)
      case instr("acc", "-", offset) => execute(idx + 1, seen + idx, acc - offset.toInt)
    }

execute()

def recover(idx: Int, seen: Set[Int], acc: Int, flipped: Boolean): Option[Int] =
  if (idx == input.length) Some(acc)
  else if (idx > input.length - 1 || seen.contains(idx)) None
  else
    input(idx) match {
      case instr("nop", "+", _) if flipped =>
        recover(idx + 1, seen + idx, acc, flipped)

      case instr("nop", "+", offset) =>
        recover(idx + offset.toInt, seen + idx, acc, flipped = true)
          .orElse(recover(idx + 1, seen + idx, acc, flipped))

      case instr("nop", "-", _) if flipped =>
        recover(idx + 1, seen + idx, acc, flipped)

      case instr("nop", "-", offset) =>
        recover(idx - offset.toInt, seen + idx, acc, flipped = true)
          .orElse(recover(idx + 1, seen + idx, acc, flipped))

      case instr("jmp", "+", offset) if flipped =>
        recover(idx + offset.toInt, seen + idx, acc, flipped)

      case instr("jmp", "+", offset) =>
        recover(idx + 1, seen + idx, acc, flipped = true)
          .orElse(recover(idx + offset.toInt, seen + idx, acc, flipped))

      case instr("jmp", "-", offset) if flipped =>
        recover(idx - offset.toInt, seen + idx, acc, flipped)

      case instr("jmp", "-", offset) =>
        recover(idx + 1, seen + idx, acc, flipped = true)
          .orElse(recover(idx - offset.toInt, seen + idx, acc, flipped))

      case instr("acc", "+", offset) =>
        recover(idx + 1, seen + idx, acc + offset.toInt, flipped)

      case instr("acc", "-", offset) =>
        recover(idx + 1, seen + idx, acc - offset.toInt, flipped)
    }

recover(0, Set(), 0, flipped = false)
