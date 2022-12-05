val input = scala.io.Source.fromResource(s"advent2022/day05.txt").getLines().toSeq

val stacksInput = input.takeWhile(_.nonEmpty)
val indices     = stacksInput.last.zipWithIndex.collect { case (pos, idx) if pos.isDigit => (pos.toString, idx) }.toMap
val actions     = input.takeRight(input.length - (stacksInput.size + 1))

val stacks = stacksInput.init
  .foldLeft(indices.map { case (pos, _) => (pos, Vector.empty[Char]) }) { case (stacks, row) =>
    stacks.map {
      case (pos, stack) if indices(pos) < row.length && row(indices(pos)).isLetter => (pos, stack :+ row(indices(pos)))
      case (pos, stack)                                                            => (pos, stack)
    }
  }
  .view
  .mapValues(_.toList)
  .toMap

// part 1
actions
  .foldLeft(stacks) { case (stacks, s"move $count from $from to $to") =>
    stacks
      .updated(to, stacks(from).take(count.toInt).reverse ++ stacks(to))
      .updated(from, stacks(from).drop(count.toInt))
  }
  .toSeq
  .sortBy(_._1)
  .map(_._2.head)
  .mkString

// part 2 (the only different is not reversing cargo)
actions
  .foldLeft(stacks) { case (stacks, s"move $count from $from to $to") =>
    stacks
      .updated(to, stacks(from).take(count.toInt) ++ stacks(to))
      .updated(from, stacks(from).drop(count.toInt))
  }
  .toSeq
  .sortBy(_._1)
  .map(_._2.head)
  .mkString
