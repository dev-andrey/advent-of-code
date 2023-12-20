val lines = scala.io.Source.fromResource(s"advent2023/day12.txt").getLines().toSeq

case class Record(springs: List[Char], check: List[Int]):
  def unfold = Record(
    (1 to 5).map(_ => springs).reduce((s1, s2) => s1 ++ ('?' :: s2)),
    (1 to 5).flatMap(_ => check).toList
  )

def memo[K, V](f: K => V): K => V =
  val cache = collection.mutable.Map.empty[K, V]
  key => cache.getOrElseUpdate(key, f(key))

lazy val arrangements: Record => Long = memo {
  case Record(springs, Nil) if springs.contains('#') => 0
  case Record(springs, Nil)                          => 1
  case Record(Nil, _ :: _)                           => 0

  case Record('.' :: springsTail, checks) =>
    arrangements(Record(springsTail, checks))

  case Record('?' :: springsTail, checks) =>
    arrangements(Record(springsTail, checks)) + arrangements(Record('#' :: springsTail, checks))

  case Record('#' :: springsTail, hashCount :: remainingChecks) =>
    if springsTail.length < hashCount - 1 then 0
    else
      val (hashes, remaining) = springsTail.splitAt(hashCount - 1)
      if hashes.contains('.') then 0
      else
        remaining match
          case Nil                 => arrangements(Record(Nil, remainingChecks))
          case ('.' | '?') :: next => arrangements(Record(next, remainingChecks))
          case _                   => 0

  case _ => 0 // impossible!!!
}

val records = lines
  .map(str => str.split("\\s"))
  .map(arr => Record(arr.head.toList, arr.last.split(",").map(_.toInt).toList))

val part1 = records
  .map(arrangements)
  .sum

val part2 = records
  .map(_.unfold)
  .map(arrangements)
  .sum
