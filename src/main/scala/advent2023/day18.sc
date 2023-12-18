val lines = scala.io.Source.fromResource(s"advent2023/day18.txt").getLines().toSeq

case class Loc(x: Long, y: Long):
  def increase(offset: Long) = this.copy(x = x * offset, y = y * offset)
  def +(other: Loc)          = this.copy(x = x + other.x, y = y + other.y)

def direction(dir: Char) =
  dir match
    case 'R' | '0' => Loc(1, 0)
    case 'U' | '1' => Loc(0, -1)
    case 'L' | '2' => Loc(-1, 0)
    case 'D' | '3' => Loc(0, 1)

def calcLavaArea(plan: Seq[(Loc, Long)]): Long =
  @scala.annotation.tailrec
  def digTrench(current: Loc, plan: Seq[(Loc, Long)], vertices: Seq[Loc]): Seq[Loc] =
    if plan.isEmpty then vertices
    else
      val (dir, meters) = plan.head
      val nextVertex    = current + dir.increase(meters)
      digTrench(nextVertex, plan.tail, vertices :+ current)

  def calcPerimeter(vertices: Seq[Loc]) =
    (vertices zip (vertices.tail :+ vertices.head)).map { case (a, b) =>
      val dx = b.x - a.x
      val dy = b.y - a.y
      math.sqrt(dx * dx + dy * dy)
    }.sum

  def shoelaceArea(vertices: Seq[Loc]) =
    val n    = vertices.length
    val sum1 = (0 until n - 1).map(i => vertices(i).x * vertices(i + 1).y).sum + vertices(n - 1).x * vertices.head.y
    val sum2 = (0 until n - 1).map(i => vertices(i).y * vertices(i + 1).x).sum + vertices(n - 1).y * vertices.head.x
    0.5 * (sum1 - sum2).abs

  val vertices  = digTrench(Loc(0L, 0L), plan, Seq.empty)
  val perimeter = calcPerimeter(vertices)
  val area      = shoelaceArea(vertices)

  (area + perimeter / 2 + 1).toLong

// part 1
val simplePlan = lines.map { case s"${d} ${m} (${_})" => (direction(d.head), m.toLong) }
val part1      = calcLavaArea(simplePlan)

// part 2
val megaPlan = lines.map { case s"${_} ${_} (#${hex})" => (direction(hex.last), BigInt(hex.take(5), 16).toLong) }
val part2    = calcLavaArea(megaPlan)
