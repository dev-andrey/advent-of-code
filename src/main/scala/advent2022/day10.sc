val input = scala.io.Source.fromResource(s"advent2022/day10.txt").getLines().toSeq

val memory = input.foldLeft(Vector(1)) {
  case memory -> s"addx $num" => memory :+ memory.last :+ (memory.last + num.toInt)
  case memory -> s"noop"      => memory :+ memory.last
}

// part 1
(20 to 220 by 40).map(cycle => cycle * memory(cycle - 1)).sum

// part 2
memory.zipWithIndex
  .map {
    case pos -> idx if Set(pos - 1, pos, pos + 1) contains idx % 40 => '#'
    case _ => '.'
  }
  .grouped(40)
  .map(_.mkString)
  .mkString("\n")
