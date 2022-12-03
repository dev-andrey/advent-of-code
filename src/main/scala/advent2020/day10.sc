val input = scala.io.Source.fromResource(s"advent2020/day10.txt").getLines().map(_.toInt).toVector

val maxAdapter = input.max
input.sorted
  .prepended(0)
  .appended(maxAdapter + 3)
  .sliding(2)
  .map(diff => diff.last - diff.head)
  .toList
  .groupBy(identity)
  .filter(_._1 != 2)
  .map(_._2.size)
  .product

// part 2
def memo[K, V](f: K => V): K => V = {
  val cache = scala.collection.mutable.Map.empty[K, V]
  k => cache.getOrElseUpdate(k, f(k))
}

val adapters = input.sorted
lazy val find: Int => Long = memo {
  case current if current == maxAdapter => 1L
  case current                          => adapters.filter(a => a > current && a <= current + 3).map(find).sum
}

find(0)
