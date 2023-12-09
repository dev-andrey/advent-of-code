val lines = scala.io.Source.fromResource(s"advent2023/day08.txt").getLines().toSeq

val directions = lines.head
val network    = lines
  .drop(2)
  .map { case s"$node = ($left, $right)" =>
    node -> (left, right)
  }
  .toMap

def walk(node: String, finish: String => Boolean, step: Int): Int =
  if finish(node) then step
  else
    directions(step % directions.length) match
      case 'L' => walk(network(node)._1, finish, step + 1)
      case 'R' => walk(network(node)._2, finish, step + 1)

val part1 = walk("AAA", node => node == "ZZZ", 0)

def gcd(a: Long, b: Long): Long = if b == 0 then scala.math.abs(a) else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = scala.math.abs(a * b) / gcd(a, b)

val part2 = network.keys
  .filter(_.last == 'A')
  .map(node => walk(node, _.endsWith("Z"), 0).toLong)
  .reduce(lcm)
