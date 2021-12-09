val raw = scala.io.Source.fromResource(s"advent2021/day08.txt").getLines()

val input = raw
  .map(_.split(" \\| "))
  .map(line => (line.head.split(' ').toList, line.last.split(' ').toList))
  .toList

def decode(signals: List[String], outputs: List[String]): Int = {
  val evidence = signals.map(_.toSet) ++ outputs.map(_.toSet)

  val facts = evidence.collect {
    case digit if digit.size == 2 => 1 -> digit
    case digit if digit.size == 4 => 4 -> digit
    case digit if digit.size == 3 => 7 -> digit
    case digit if digit.size == 7 => 8 -> digit
  }.toMap

  val _1 = facts(1)
  val _4 = facts(4)
  val _7 = facts(7)
  val _8 = facts(8)

  val fives = evidence.filter(_.size == 5)
  val sixes = evidence.filter(_.size == 6)

  val top              = _7 diff _1
  val topLeftMiddle    = _4 diff _1
  val bottomLeftBottom = _8 diff _4 diff top

  val _5          = fives.find(dig => (dig intersect topLeftMiddle).size == 2).get
  val bottomLeft  = bottomLeftBottom diff _5
  val topRight    = _1 diff _5
  val bottomRight = _1 diff topRight
  val _9          = sixes.find(dig => (dig intersect bottomLeftBottom).size == 1).get
  val _6          = _5 union bottomLeft
  val middle      = fives.map(dig => dig intersect topLeftMiddle).find(_.size == 1).get
  val topLeft     = topLeftMiddle diff middle
  val _0          = _8 diff middle
  val _2          = _8 diff topLeft diff bottomRight
  val _3          = _8 diff topLeft diff bottomLeft

  val code = Map(
    _0 -> '0',
    _1 -> '1',
    _2 -> '2',
    _3 -> '3',
    _4 -> '4',
    _5 -> '5',
    _6 -> '6',
    _7 -> '7',
    _8 -> '8',
    _9 -> '9'
  )

  outputs.map(output => code(output.toSet)).mkString.toInt
}

input.map { case (signals, outputs) =>
  decode(signals, outputs)
}.sum
