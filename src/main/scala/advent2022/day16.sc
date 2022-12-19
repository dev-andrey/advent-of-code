val input = scala.io.Source.fromResource(s"advent2022/day16.txt").getLines()

val (pressures, tunnels) = input
  .foldLeft((Map.empty[String, Int], Map.empty[String, Seq[String]])) {
    case (pressure, tunnels) -> s"Valve $id has flow rate=$flow; tunnel leads to valve $valve" =>
      (pressure + (id -> flow.toInt), tunnels + (id -> Seq(valve)))
    case (pressure, tunnels) -> s"Valve $id has flow rate=$flow; tunnels lead to valves $valves" =>
      (pressure + (id -> flow.toInt), tunnels + (id -> valves.split(',').map(_.trim)))
  }

val openable = pressures.filter(_._2 > 0).keySet

// Slow, need to explore proper algorithms
def shortestPaths(valves: Set[String]): Map[String, Map[String, Int]] = {
  final case class Path(curr: String, visited: List[String] = List.empty)

  def shortestPath(paths: Seq[Path], target: String, minDistance: Int = Int.MaxValue): Int = {
    val (arrived, exploring) = paths.partition(_.curr == target)
    val shortest             = arrived.map(_.visited.size).minOption.getOrElse(Int.MaxValue) min minDistance
    val toExplore            = exploring.filter(_.visited.size < shortest)

    if (toExplore.isEmpty) shortest
    else
      shortestPath(
        toExplore.flatMap { case Path(curr, visited) =>
          tunnels(curr).map(next => Path(next, curr :: visited))
        },
        target,
        shortest
      )
  }

  valves.foldLeft(Map.empty[String, Map[String, Int]]) { case res -> valve =>
    val paths =
      (valves - valve).map(target => target -> shortestPath(Seq(Path(valve)), target)).toMap
    res + (valve -> paths)
  }
}

val distances = shortestPaths(pressures.keySet)

final case class State(time: Int, current: String, closed: Set[String], pressure: Int)

def simulate(exploring: Seq[State], openable: Set[String], currMax: Int): Int = {
  val (finished, progress) = exploring.partition(sim => sim.closed.isEmpty || sim.time == 0)
  val nextMax = currMax max finished
    .map {
      case State(0, _, _, pressure)        => pressure
      case State(timeLeft, _, _, pressure) => openable.map(pressures).sum * timeLeft + pressure
    }
    .maxOption
    .getOrElse(currMax)

  if (progress.isEmpty) nextMax
  else {
    val nextSimulation = progress.flatMap {
      case State(timeLeft, current, closed, pressure) if closed contains current =>
        Seq(State(timeLeft - 1, current, closed - current, pressure + (openable -- closed).map(pressures).sum))

      case State(timeLeft, current, closed, pressure) =>
        closed.map { toOpen =>
          val distance = distances(current)(toOpen)
          if (timeLeft - distance < 0)
            State(0, toOpen, closed, pressure + (openable -- closed).map(pressures).sum * timeLeft)
          else
            State(timeLeft - distance, toOpen, closed, pressure + (openable -- closed).map(pressures).sum * distance)
        }
    }

    simulate(nextSimulation, openable, nextMax)
  }
}

// part 1
simulate(Seq(State(30, "AA", openable, 0)), openable, 0)

// part 2
def arrangeOpenning(valves: List[String]): List[(List[String], List[String])] =
  if (valves.isEmpty) List((Nil, Nil))
  else
    arrangeOpenning(valves.tail).flatMap { case (human, elephant) =>
      List((valves.head :: human, elephant), (human, valves.head :: elephant))
    }

arrangeOpenning(openable.toList).foldLeft(0) { case currentMax -> ((humanOpens, elephantOpens)) =>
  val maxHuman    = simulate(Seq(State(26, "AA", humanOpens.toSet, 0)), humanOpens.toSet, 0)
  val maxElephant = simulate(Seq(State(26, "AA", elephantOpens.toSet, 0)), elephantOpens.toSet, 0)
  currentMax max (maxHuman + maxElephant)
}
