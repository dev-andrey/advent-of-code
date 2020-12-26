val input = scala.io.Source.fromResource(s"advent2020/day07.txt").getLines()

val rules = input.flatMap { line =>
  val rule = "(.*) bag[s]? contain (.*)".r
  line match {
    case rule(bag, content) =>
      Map(
        bag.trim -> "(\\d+ \\w+ \\w+)"
          .r
          .findAllMatchIn(content)
          .map(_.matched)
          .map(b => b.dropWhile(_.isDigit).trim -> b.takeWhile(_.isDigit).toInt)
          .toMap
      )
  }
}.toMap

val containMap = rules.foldLeft(rules.keys.map(_ -> Set.empty[String]).toMap) { case (output, (bag, inside)) =>
  output.filter(rec => inside.keySet.contains(rec._1)).map { case b -> outside =>
    b -> (outside + bag)
  } ++ (output -- inside.keySet)
}

@annotation.tailrec
def findContainer(search: Vector[String], containers: Set[String] = Set()): Set[String] =
  if (search.isEmpty) containers
  else
    containMap.get(search.head) match {
      case None => findContainer(search.tail, containers)
      case Some(bags) => findContainer(search.tail ++ bags, containers ++ bags)
    }

findContainer(Vector("shiny gold")).size

def countBags(bag: String): Int =
  rules.get(bag) match {
    case Some(inside) =>
      if (inside.isEmpty) 0
      else
        inside.map { case (b, cnt) =>
          cnt + cnt * countBags(b)
        }.sum
    case None => 1
  }

countBags("shiny gold")
