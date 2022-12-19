val input = scala.io.Source.fromResource(s"advent2022/day18.txt").getLines().toSeq

val offsets = Seq((-1, 0, 0), (0, -1, 0), (0, 0, -1), (1, 0, 0), (0, 1, 0), (0, 0, 1))
final case class Cube(x: Int, y: Int, z: Int) { self =>
  def nearby = offsets.map { case (dx, dy, dz) => Cube(x + dx, y + dy, z + dz) }
}

val cubes = input.map(str => str.split(",")).map(arr => Cube(arr(0).toInt, arr(1).toInt, arr(2).toInt)).toSet

// part 1
cubes.toSeq.map(_.nearby.filterNot(cubes).size).sum

// part 2
val airSet = {
  for {
    x <- cubes.map(_.x).min - 1 to cubes.map(_.x).max + 1
    y <- cubes.map(_.y).min - 1 to cubes.map(_.y).max + 1
    z <- cubes.map(_.z).min - 1 to cubes.map(_.z).max + 1
    cube = Cube(x, y, z)
    if !cubes.contains(cube)
  } yield cube
}.toSet

def search(pending: Vector[Cube], visited: Set[Cube]): Set[Cube] =
  if (pending.isEmpty) visited
  else {
    val next = pending.head.nearby.filterNot(visited).filter(airSet)
    search(pending.tail ++ next, visited ++ next)
  }

val outside = search(Vector(airSet.head), Set(airSet.head))
cubes.toSeq.map(_.nearby.count(outside)).sum
