val input = scala.io.Source.fromResource(s"advent2022/day11.txt").getLines().toSeq

final case class Monkey(items: Seq[Long], operation: Long => Long, divisibleBy: Long, ifTrue: Int, ifFalse: Int)
val monkeys = input
  .filter(_.nonEmpty)
  .grouped(6)
  .map { monkey =>
    val monkeyNo = monkey.head(7).asDigit
    val items    = monkey(1).substring(18).split(',').map(_.trim).map(_.toLong).toList
    val op = monkey(2).substring(13).trim match {
      case "new = old * old"   => (item: Long) => item * item
      case s"new = old * $num" => (item: Long) => item * num.toInt
      case s"new = old + $num" => (item: Long) => item + num.toInt
    }
    val test = monkey(3).substring(21).trim.toLong

    monkeyNo -> Monkey(items, op, test, monkey(4).last.asDigit, monkey(5).last.asDigit)
  }
  .toMap

@scala.annotation.tailrec
def simulate(
  roundsLeft: Int,
  reducer: Long => Long,
  state: Map[Int, Monkey],
  counters: Map[Int, Long]
): Map[Int, Long] =
  if (roundsLeft == 0) counters
  else {
    val (newState, newCounters) = state.keys.toSeq.sorted.foldLeft((state, counters)) {
      case (business, counter) -> id =>
        val monkey              = business(id)
        val inspected           = monkey.items.map(monkey.operation)
        val (wasTrue, wasFalse) = inspected.map(reducer).partition(_ % monkey.divisibleBy == 0)
        val newBusiness = business
          .updated(id, business(id).copy(items = Seq.empty))
          .updated(monkey.ifTrue, business(monkey.ifTrue).copy(items = business(monkey.ifTrue).items ++ wasTrue))
          .updated(monkey.ifFalse, business(monkey.ifFalse).copy(items = business(monkey.ifFalse).items ++ wasFalse))

        (newBusiness, counter.updated(id, counter(id) + inspected.size))
    }

    simulate(roundsLeft - 1, reducer, newState, newCounters)
  }

// part 1
simulate(20, _ / 3, monkeys, monkeys.keys.map(_ -> 0L).toMap).values.toSeq.sorted
  .takeRight(2)
  .product

// part 2
val lcm = monkeys.map(_._2.divisibleBy).product
simulate(10_000, _ % lcm, monkeys, monkeys.keys.map(_ -> 0L).toMap).values.toSeq.sorted
  .takeRight(2)
  .product
