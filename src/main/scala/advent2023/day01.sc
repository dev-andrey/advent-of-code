val lines = scala.io.Source.fromResource(s"advent2023/day01.txt").getLines().toList

val part1 =
  lines.flatMap { line =>
    for
      first <- line.find(_.isDigit)
      last  <- line.findLast(_.isDigit)
    yield s"$first$last".toInt
  }.sum

val numMap = Map(
  "one"   -> 1,
  "two"   -> 2,
  "three" -> 3,
  "four"  -> 4,
  "five"  -> 5,
  "six"   -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine"  -> 9
)

def extract(line: String, idx: Int, backwards: Boolean): Option[Int] =
  if idx == line.length then None
  else
    val substr = line.substring(idx)
    if substr.head.isDigit then Some(substr.head.asDigit)
    else
      numMap
        .collectFirst { case (str, value) if substr.startsWith(str) => value }
        .orElse(extract(line, if backwards then idx - 1 else idx + 1, backwards))

val part2 =
  lines.flatMap { line =>
    for
      first <- extract(line, 0, false)
      last  <- extract(line, line.length - 1, true)
    yield s"$first$last".toInt
  }.sum
