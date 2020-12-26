val input = "0,14,6,20,1,4".split(',').map(_.toInt)

val init = input.zipWithIndex.map { case (num, turn) => (num, turn + 1) }.toMap

(input.length + 1 until 30_000_000)
  .foldLeft((init, 0)) { case (cache, num) -> turn =>
    (cache + (num -> turn), if (cache.contains(num)) turn - cache(num) else 0)
  }
  ._2
