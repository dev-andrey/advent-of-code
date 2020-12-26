val input = scala.io.Source.fromResource(s"advent2020/day19.txt").getLines()

val (rules, data) = input.toList.filter(_.nonEmpty).span(_.head.isDigit)
val ruleMap       = rules.map(_.split(':')).map(arr => arr.head -> arr.last.trim).toMap

def memo[K, V](f: K => V): K => V = {
  val cache = collection.mutable.Map.empty[K, V]
  key => cache.getOrElseUpdate(key, f(key))
}

lazy val makeRule: String => String = memo {
  case "0" =>
    {
      for {
        r8count  <- 1 to 10
        r11count <- 1 to 10
      } yield s"${makeRule("42")}{$r8count}(?<left>${makeRule("42")}{$r11count})(?<right>${makeRule("31")}{$r11count})"
    }.mkString(";;") // to split later

  case id =>
    ruleMap(id) match {
      case rule if rule.contains('"') =>
        s"${rule.replace("\"", "")}"
      case rule if rule.contains("|") =>
        rule.split('|').map(rs => rs.trim.split(' ').map(r => makeRule(r.trim)).mkString("")).mkString("(?>", "|", ")")
      case rule =>
        rule.split(' ').map(r => makeRule(r.trim)).mkString("(?>", "", ")")
    }
}

val regexes = makeRule("0").split(";;").map(_.r)

data.count { row =>
  regexes.exists(regex =>
    regex.matches(row) && regex.findAllMatchIn(row).exists(m => m.group("left").length == m.group("right").length)
  )
}

// case "0" => s"${makeRule("8")}${makeRule("11")}"
// case "8"  => s"(?<rule8>${makeRule("42")}+)"
// case "11" => s"(?<rule11>(?<left>${makeRule("42")}+)(?<right>${makeRule("31")}+))"
