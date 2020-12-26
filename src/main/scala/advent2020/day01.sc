val expenses = scala.io.Source.fromResource(s"advent2020/day01.txt").getLines().map(_.toInt).toSet

expenses.map(2020 - _).intersect(expenses).product
expenses.flatMap(expense => expenses.map(2020 - expense - _).intersect(expenses)).product
