val lines = scala.io.Source.fromResource(s"advent2023/day07.txt").getLines().toSeq

case class Hand(cards: String, bid: Int):
  val codes = cards
    .replace('A', 'Z')
    .replace('K', 'Y')
    .replace('Q', 'X')
    .replace('J', 'W')
    .replace('T', 'V')

  def getHandValue(cards: String): Int =
    val grouped = cards.groupMapReduce(identity)(_ => 1)(_ + _)

    if grouped.values.count(_ == 5) == 1 then 6      // five of a kind
    else if grouped.values.count(_ == 4) == 1 then 5 // four of a kind
    else if grouped.values.toSet == Set(2, 3) then 4 // full house
    else if grouped.values.count(_ == 3) == 1 then 3 // three of a kind
    else if grouped.values.count(_ == 2) == 2 then 2 // two pairs
    else if grouped.values.count(_ == 2) == 1 then 1 // one pair
    else 0                                           // high card

  val strength: Int = getHandValue(cards)

  val jokerStrength: Int =
    if cards.count(_ == 'J') == 5 then 6
    else if cards.contains('J') then
      val jokerCount = cards.count(_ == 'J')
      val cardMap    = cards.filter(_ != 'J').groupMapReduce(identity)(_ => 1)(_ + _)
      val bigCard    = cardMap.maxBy(_._2)._1
      getHandValue(cards.replace('J', bigCard))
    else strength

val hands = lines
  .map(line => Hand(line.takeWhile(_ != ' '), line.dropWhile(_ != ' ').trim.toInt))

val normalOrdering: Ordering[Hand] = (left: Hand, right: Hand) =>
  if left.strength > right.strength then 1
  else if left.strength < right.strength then -1
  else left.codes compareTo right.codes

val part1 = hands
  .sorted(normalOrdering)
  .zipWithIndex
  .map { case (Hand(_, bid), rank) => bid * (rank + 1) }
  .sum

val trickyOrdering: Ordering[Hand] = (left: Hand, right: Hand) =>
  if left.jokerStrength > right.jokerStrength then 1
  else if left.jokerStrength < right.jokerStrength then -1
  else left.codes.replace('W', '1') compareTo right.codes.replace('W', '1')

val part2 = hands
  .sorted(trickyOrdering)
  .zipWithIndex
  .map { case (Hand(_, bid), rank) => bid * (rank + 1) }
  .sum
