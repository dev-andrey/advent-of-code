val lines = scala.io.Source.fromResource(s"advent2023/day02.txt").getLines().toList

case class RGB(red: Int, green: Int, blue: Int):
  def power = red * green * blue

case class Game(num: Int, cubes: List[RGB])

def extractRgb(str: String): RGB =
  val pattern = "(\\d+)\\s*(red|green|blue)".r
  val matches = pattern.findAllMatchIn(str).map(m => (m.group(2), m.group(1).toInt)).toMap
  RGB(matches.getOrElse("red", 0), matches.getOrElse("green", 0), matches.getOrElse("blue", 0))

val games = lines.map { case s"Game ${num}: ${sets}" =>
  Game(
    num.toInt,
    sets.split(';').map(extractRgb).toList
  )
}

val part1 = games
  .filter(_.cubes.forall(rgb => rgb.red <= 12 && rgb.green <= 13 && rgb.blue <= 14))
  .map(_.num)
  .sum

val part2 = games
  .map(_.cubes.reduce { (acc, rgb) =>
    RGB(acc.red max rgb.red, acc.green max rgb.green, acc.blue max rgb.blue)
  })
  .map(_.power)
  .sum
