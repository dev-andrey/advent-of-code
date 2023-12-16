val input = scala.io.Source.fromResource(s"advent2023/day15.txt").mkString.split(",").toSeq

def hash(str: String): Int =
  str.foldLeft(0) { case current -> ch =>
    (current + ch.toInt) * 17 % 256
  }

val part1 = input.map(hash).sum

case class Lens(label: String, focalLength: Int)

val part2 = input
  .foldLeft(Seq.fill[Seq[Lens]](256)(Seq())) {
    case boxes -> s"$label=$length" =>
      val boxIdx = hash(label)
      val lenses = boxes(boxIdx)

      boxes.updated(
        boxIdx,
        lenses.indexWhere(_.label == label) match
          case -1  => lenses :+ Lens(label, length.toInt)
          case idx => lenses.updated(idx, Lens(label, length.toInt))
      )

    case boxes -> s"$label-" =>
      val boxIdx = hash(label)
      val lenses = boxes(boxIdx)
      boxes.updated(
        boxIdx,
        lenses.filterNot(_.label == label)
      )
  }
  .zipWithIndex
  .filterNot(_._1.isEmpty)
  .flatMap { case (lenses, boxIdx) =>
    lenses.zipWithIndex.map { case (lens, slot) =>
      (1 + boxIdx) * (1 + slot) * lens.focalLength
    }
  }
  .sum
