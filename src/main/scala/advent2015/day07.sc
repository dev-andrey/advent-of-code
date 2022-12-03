val input = scala.io.Source.fromResource(s"advent2015/day07.txt").getLines().toSeq

sealed trait Circuit
object Circuit {
  final case class Wire(input: String)                   extends Circuit
  final case class Not(label: String)                    extends Circuit
  final case class And(left: String, right: String)      extends Circuit
  final case class Or(left: String, right: String)       extends Circuit
  final case class LeftShift(label: String, shift: Int)  extends Circuit
  final case class RightShift(label: String, shift: Int) extends Circuit
}

def makeCircuit(scheme: Seq[String]) =
  scheme.map {
    case s"$left AND $right -> $target"      => target -> Circuit.And(left, right)
    case s"$left OR $right -> $target"       => target -> Circuit.Or(left, right)
    case s"$signal LSHIFT $shift -> $target" => target -> Circuit.LeftShift(signal, shift.toInt)
    case s"$signal RSHIFT $shift -> $target" => target -> Circuit.RightShift(signal, shift.toInt)
    case s"NOT $signal -> $target"           => target -> Circuit.Not(signal)
    case s"$signal -> $target"               => target -> Circuit.Wire(signal)
  }.toMap

def memo[K, V](f: K => V): K => V = {
  val cache = collection.mutable.Map.empty[K, V]
  key => cache.getOrElseUpdate(key, f(key))
}

def verify(scheme: Seq[String]) = {
  val circuit = makeCircuit(scheme)

  lazy val checkSignal: String => Int = memo { out =>
    def valueOf(signal: String): Int = signal.toIntOption getOrElse checkSignal(signal)

    circuit(out) match {
      case Circuit.Wire(in)              => valueOf(in)
      case Circuit.Not(in)               => ~valueOf(in)
      case Circuit.And(left, right)      => valueOf(left) & valueOf(right)
      case Circuit.Or(left, right)       => valueOf(left) | valueOf(right)
      case Circuit.LeftShift(in, shift)  => valueOf(in) << shift
      case Circuit.RightShift(in, shift) => valueOf(in) >> shift
    }
  }

  checkSignal("a")
}

// part 1
verify(input)

// part 2
verify(input :+ "956 -> b")
