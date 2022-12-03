val input = scala.io.Source.fromResource(s"advent2020/day20.txt").getLines()

val tileCoordMap = input
  .filter(_.nonEmpty)
  .foldLeft(List.empty[(Int, Map[(Int, Int), Char])]) {
    case out -> line if line.startsWith("Tile") =>
      line.drop(5).dropRight(1).toInt -> Map.empty[(Int, Int), Char] :: out
    case ((id -> coords) :: tail) -> line =>
      val updated = line.zipWithIndex.foldLeft(coords) { case (map, (ch, x)) =>
        map + ((x, coords.keySet.map(_._2).size) -> ch)
      }
      (id -> updated) :: tail
  }
  .toMap

val size   = 10
val maxIdx = size - 1

final case class Tile(get: (Int, Int) => Char) { self =>
  def rotate(angle: Int): Tile =
    angle match {
      case 0   => self
      case 90  => Tile((x, y) => self.get(y, maxIdx - x))
      case 180 => Tile((x, y) => self.get(maxIdx - x, maxIdx - y))
      case 270 => Tile((x, y) => self.get(maxIdx - y, x))
    }

  def flip(mirror: Boolean): Tile =
    if (mirror) Tile((x, y) => self.get(maxIdx - x, y))
    else self

  def left: String   = (0 to maxIdx).map(y => self.get(0, y)).mkString
  def top: String    = (0 to maxIdx).map(x => self.get(x, 0)).mkString
  def right: String  = (0 to maxIdx).map(y => self.get(maxIdx, y)).mkString
  def bottom: String = (0 to maxIdx).map(x => self.get(x, maxIdx)).mkString

  def topFits(other: Tile): Boolean  = self.top == other.bottom
  def leftFits(other: Tile): Boolean = self.left == other.right

  override def toString = prettyPrint

  def prettyPrint: String =
    (0 to maxIdx).map(y => (0 to maxIdx).map(x => self.get(x, y)).mkString).mkString("\n")

}
val tileMap    = tileCoordMap.map { case (id, coordMap) => id -> Tile((x, y) => coordMap((x, y))) }
val tileCount  = tileMap.keySet.size
val tilePerRow = math.sqrt(tileCount).toInt

final case class Piece(id: Int, angle: Int, mirror: Boolean) {
  def tile: Tile = tileMap(id).rotate(angle).flip(mirror)
}

def simulate(x: Int, y: Int, unused: Map[Int, Tile], result: Map[(Int, Int), Piece]): Map[(Int, Int), Piece] = {

  def findFit(leftPiece: Option[Piece], topPiece: Option[Piece], pieces: Set[Piece]): Set[Piece] =
    (leftPiece, topPiece) match {
      case (None, None)       => pieces
      case (Some(left), None) => pieces.filter(_.tile.leftFits(left.tile))
      case (None, Some(top))  => pieces.filter(_.tile.topFits(top.tile))
      case (Some(left), Some(top)) =>
        pieces.filter(pos => pos.tile.leftFits(left.tile) && pos.tile.topFits(top.tile))
    }

  def positions(tiles: Map[Int, Tile]): Set[Piece] = {
    for {
      tile   <- tiles
      angle  <- List(0, 90, 180, 270)
      mirror <- List(true, false)
    } yield Piece(tile._1, angle, mirror)
  }.toSet

  if (unused.isEmpty) result
  else
    findFit(result.get(x - 1, y), result.get(x, y - 1), positions(unused)) match {
      case candidates if candidates.isEmpty => Map.empty[(Int, Int), Piece]

      case candidates if x == tilePerRow - 1 =>
        candidates
          .map(piece => (x, y) -> piece)
          .toVector
          .flatMap { case (loc, piece) =>
            simulate(0, y + 1, unused - piece.id, result + ((loc, piece)))
          }
          .toMap

      case candidates =>
        candidates
          .map(piece => (x, y) -> piece)
          .toVector
          .flatMap { case (loc, piece) =>
            simulate(x + 1, y, unused - piece.id, result + ((loc, piece)))
          }
          .toMap
    }
}

val result = simulate(0, 0, tileMap, Map.empty[(Int, Int), Piece])
val removedBorders = (0 until tilePerRow * size)
  .filter(row => row % 10 != 0 && row % 10 != 9)
  .map { y =>
    (0 until tilePerRow * size)
      .filter(row => row % 10 != 0 && row % 10 != 9)
      .map { x =>
        result(x / size, y / size).tile.get(x % size, y % size)
      }
  }

final case class Image(get: (Int, Int) => Char) { self =>
  def rotate(angle: Int): Image =
    angle match {
      case 0   => self
      case 90  => Image((x, y) => self.get(y, tilePerRow * 8 - 1 - x))
      case 180 => Image((x, y) => self.get(tilePerRow * 8 - 1 - x, tilePerRow * 8 - 1 - y))
      case 270 => Image((x, y) => self.get(tilePerRow * 8 - 1 - y, x))
    }

  def rotate: Image = rotate(90)

  def flip(mirror: Boolean): Image =
    if (mirror) Image((x, y) => self.get(tilePerRow * 8 - 1 - x, y))
    else self

  def monsterCount: Int =
    (for {
      x <- 0 until tilePerRow * 8 - 20
      y <- 1 until tilePerRow * 8 - 1
      if get(x, y) == '#'
    } yield (x, y)).count { case (x, y) =>
      List(
        get(x, y),
        get(x + 1, y + 1),
        get(x + 4, y + 1),
        get(x + 5, y),
        get(x + 6, y),
        get(x + 7, y + 1),
        get(x + 10, y + 1),
        get(x + 11, y),
        get(x + 12, y),
        get(x + 13, y + 1),
        get(x + 16, y + 1),
        get(x + 17, y),
        get(x + 18, y),
        get(x + 18, y - 1),
        get(x + 19, y)
      ).forall(_ == '#')
    }

  def waterDensity: Int = {
    for {
      x <- 0 until tilePerRow * 8
      y <- 0 until tilePerRow * 8
      if get(x, y) == '#'
    } yield 1
  }.sum - monsterCount * 15

  override def toString = prettyPrint

  def prettyPrint: String =
    (0 until tilePerRow * 8)
      .map { y =>
        (0 until tilePerRow * 8).map { x =>
          get(x, y)
        }.mkString
      }
      .mkString("\n")
}

val img = Image((x, y) => removedBorders(y)(x))

val waterDensities = for {
  angle  <- List(0, 90, 180, 270)
  mirror <- List(true, false)
} yield img.rotate(angle).flip(mirror).waterDensity

waterDensities.min
