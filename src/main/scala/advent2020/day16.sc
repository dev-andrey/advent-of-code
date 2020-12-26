val input = scala.io.Source.fromResource(s"advent2020/day16.txt").getLines().toList.filter(_.nonEmpty)

val ticketRule = "(\\D+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r

val (ruleMap, tickets) =
  input.foldLeft((Map.empty[String, Int => Boolean], Vector.empty[Vector[Int]])) { case (rules, tickets) -> line =>
    line match {
      case ticketRule(name, min1, max1, min2, max2) =>
        val rule = (a: Int) => a >= min1.toInt && a <= max1.toInt || a >= min2.toInt && a <= max2.toInt
        (rules + (name -> rule), tickets)
      case str if str.contains(',') =>
        (rules, tickets :+ str.split(',').map(_.toInt).toVector)
      case _ => (rules, tickets)
    }
  }
val myTicket      = tickets.head
val nearbyTickets = tickets.tail

// part 1
ruleMap
  .foldLeft(nearbyTickets) { case (tickets, (_, rule)) =>
    tickets.map(_.filterNot(rule))
  }
  .flatten
  .sum

// part 2
val validTickets = nearbyTickets.foldLeft(Vector.empty[Vector[Int]]) { case valid -> ticket =>
  if (
    ruleMap
      .foldLeft(ticket) { case (toValidate, (_, rule)) =>
        toValidate.filterNot(rule)
      }
      .isEmpty
  ) valid :+ ticket
  else valid
}

val guesses = validTickets
  .flatMap(_.zipWithIndex)
  .map(_.swap)
  .groupMap(_._1)(_._2)
  .map { case idx -> values =>
    idx -> ruleMap.foldLeft(Set.empty[String]) { case (out, (name, rule)) =>
      if (values.forall(rule)) out + name else out
    }
  }
  .map { case (idx, rules) => rules -> idx }

@annotation.tailrec
def deduce(facts: Map[String, Int], unknown: Map[Set[String], Int]): Map[String, Int] =
  if (unknown.isEmpty) facts
  else {
    val deduced = unknown.map { case set -> idx =>
      (set diff facts.keySet) -> idx
    }
    deduce(
      facts ++ deduced.filter(_._1.size == 1).map { case (set, idx) => set.head -> idx },
      deduced.filter(_._1.size > 1)
    )
  }

deduce(Map.empty[String, Int], guesses).collect { case (name, idx) if name.startsWith("departure") => myTicket(idx) }
  .map(_.toLong)
  .product
