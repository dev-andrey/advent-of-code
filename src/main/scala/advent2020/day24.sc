val input = scala.io.Source.fromResource(s"advent2020/day24.txt").getLines()

final case class Hex(q: Int, r: Int) { self =>
  def e: Hex  = self.copy(q = q + 1)
  def se: Hex = self.copy(r = r + 1)
  def sw: Hex = self.copy(q = q - 1, r = r + 1)
  def w: Hex  = self.copy(q = q - 1)
  def nw: Hex = self.copy(r = r - 1)
  def ne: Hex = self.copy(q = q + 1, r = r - 1)

  def neighbors: Set[Hex] = Set(e, se, sw, w, nw, ne)

  override def toString = s"[${if (q > 0) "+" + q else q} ${if (r > 0) "+" + r else r}]"
}

@annotation.tailrec
def pathToHex(in: String, out: Hex = Hex(0, 0)): Hex =
  if (in.isEmpty) out
  else if (in.startsWith("e")) pathToHex(in.drop(1), out.e)
  else if (in.startsWith("w")) pathToHex(in.drop(1), out.w)
  else
    in.splitAt(2) match {
      case "se" -> rest => pathToHex(rest, out.se)
      case "sw" -> rest => pathToHex(rest, out.sw)
      case "nw" -> rest => pathToHex(rest, out.nw)
      case "ne" -> rest => pathToHex(rest, out.ne)
    }

val identified = input
  .map(pathToHex(_))
  .toVector
  .groupMapReduce(identity)(_ => 1)(_ + _)

val blackHexes = identified.filter(_._2 % 2 != 0).keySet
s"No of black tiles: ${blackHexes.size}"

@annotation.tailrec
def flip(day: Int, blackTiles: Set[Hex]): Set[Hex] =
  if (day == 100) blackTiles
  else {
    val flippedWhite = blackTiles.filter { tile =>
      val blackAround = (tile.neighbors intersect blackTiles).size
      blackAround == 0 || blackAround > 2
    }

    val flippedBlack = blackTiles.flatMap { tile =>
      (tile.neighbors diff blackTiles).filter { whiteTile =>
        (whiteTile.neighbors intersect blackTiles).size == 2
      }
    }

    flip(day + 1, blackTiles -- flippedWhite ++ flippedBlack)
  }

flip(0, blackHexes).size
