val input = scala.io.Source.fromResource(s"advent2020/day10.txt").getLines().map(_.toInt).toVector

val maxAdapter = input.max
input
  .sorted
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

lazy val combos: ((Int, Set[Int])) => Long = memo[(Int, Set[Int]), Long] {
  case curr -> _ if curr == maxAdapter   => 1L
  case _ -> adapters if adapters.isEmpty => 0L
  case curr -> adapters =>
    (if (adapters.contains(curr + 3)) combos(curr + 3, adapters.dropWhile(_ <= curr + 3)) else 0L) +
      (if (adapters.contains(curr + 2)) combos(curr + 2, adapters.dropWhile(_ <= curr + 2)) else 0L) +
      (if (adapters.contains(curr + 1)) combos(curr + 1, adapters.dropWhile(_ <= curr + 1)) else 0L)
}
combos(0, input.toSet)
