val input = scala.io.Source.fromResource(s"advent2022/day19.txt").getLines().toSeq

final case class Minerals(ore: Int, clay: Int, obsidian: Int, geode: Int) {
  def hasEnough(other: Minerals) =
    ore - other.ore >= 0 && clay - other.clay >= 0 && obsidian - other.obsidian >= 0 && geode - other.geode >= 0

  def remove(other: Minerals) =
    Minerals(ore - other.ore, clay - other.clay, obsidian - other.obsidian, geode - other.geode)

  def add(other: Minerals) =
    Minerals(ore + other.ore, clay + other.clay, obsidian + other.obsidian, geode + other.geode)

  def hoarded = {
    val mult = 100
    Minerals(ore * mult, clay * mult, obsidian * mult, geode * mult)
  }
}

final case class Robots(ore: Int, clay: Int, obsidian: Int, geode: Int) {
  def extract = Minerals(ore, clay, obsidian, geode)

  def add(other: Robots) =
    Robots(ore + other.ore, clay + other.clay, obsidian + other.obsidian, geode + other.geode)
}

final case class Blueprint(
  id: Int,
  oreCostOre: Int,
  clayCostOre: Int,
  obsidianCostOre: Int,
  obsidianCostClay: Int,
  geodeCostOre: Int,
  geodeCostObsidian: Int
) {
  def oreRobot = Minerals(oreCostOre, 0, 0, 0)

  def clayRobot = Minerals(clayCostOre, 0, 0, 0)

  def obsidianRobot = Minerals(obsidianCostOre, obsidianCostClay, 0, 0)

  def geodeRobot = Minerals(geodeCostOre, 0, geodeCostObsidian, 0)

  val maxOreNeeded      = Seq(oreCostOre, clayCostOre, obsidianCostOre, geodeCostOre).max
  val maxClayNeeded     = obsidianCostClay
  val maxObsidianNeeded = geodeCostObsidian
}

val blueprints = input.map {
  case s"Blueprint $id: Each ore robot costs $oreCostOre ore. Each clay robot costs $clayCostOre ore. Each obsidian robot costs $obsidianCostOre ore and $obsidianCostClay clay. Each geode robot costs $geodeCostOre ore and $geodeCostObsidian obsidian." =>
    Blueprint(
      id.toInt,
      oreCostOre.toInt,
      clayCostOre.toInt,
      obsidianCostOre.toInt,
      obsidianCostClay.toInt,
      geodeCostOre.toInt,
      geodeCostObsidian.toInt
    )
}

final case class Sim(timeLeft: Int, blueprint: Blueprint, warehouse: Minerals, robots: Robots) {}

def possibleConstruction(warehouse: Minerals, blueprint: Blueprint): Seq[(Robots, Minerals)] =
  Seq(
    Robots(1, 0, 0, 0) -> blueprint.oreRobot,
    Robots(0, 1, 0, 0) -> blueprint.clayRobot,
    Robots(0, 0, 1, 0) -> blueprint.obsidianRobot,
    Robots(0, 0, 0, 1) -> blueprint.geodeRobot
  ).filter { case (_, mineralsNeeded) =>
    warehouse.hasEnough(mineralsNeeded)
  }

def canBuildGeodeInOneTurn(robots: Robots, blueprint: Blueprint) =
  robots.extract match {
    case Minerals(ore, _, obsidian, _) =>
      blueprint.geodeCostOre <= ore && blueprint.geodeCostObsidian <= obsidian
  }

def runSim(sims: Seq[Sim]): Int =
  if (sims.head.timeLeft == 0) sims.maxBy(_.warehouse.geode).warehouse.geode
  else {
    val (canBuildGeodes, running) = sims.partition(sim => canBuildGeodeInOneTurn(sim.robots, sim.blueprint))

    val toProcess = if (canBuildGeodes.nonEmpty) {
      val mostProductive = canBuildGeodes.maxBy(_.warehouse.geode)
      running.filter(_.warehouse.geode >= mostProductive.warehouse.geode) :+ mostProductive
    } else running

    val newSims = toProcess.flatMap {
      case Sim(timeLeft, blueprint, warehouse, robots) if canBuildGeodeInOneTurn(robots, blueprint) =>
        Seq(
          Sim(
            timeLeft - 1,
            blueprint,
            warehouse.remove(blueprint.geodeRobot).add(robots.extract),
            robots.add(Robots(0, 0, 0, 1))
          )
        )

      case Sim(timeLeft, blueprint, warehouse, robots) =>
        val possibleRobots = possibleConstruction(warehouse, blueprint).filter {
          case Robots(1, _, _, _) -> _ => robots.ore < blueprint.maxOreNeeded
          case Robots(_, 1, _, _) -> _ => robots.clay < blueprint.maxClayNeeded
          case Robots(_, _, 1, _) -> _ => robots.obsidian < blueprint.maxObsidianNeeded
          case _                       => true
        }

        val extracted = robots.extract

        val ifWaiting = possibleConstruction(extracted.hoarded, blueprint).filter {
          case Robots(1, _, _, _) -> _ => robots.ore < blueprint.maxOreNeeded
          case Robots(_, 1, _, _) -> _ => robots.clay < blueprint.maxClayNeeded
          case Robots(_, _, 1, _) -> _ => robots.obsidian < blueprint.maxObsidianNeeded
          case _                       => true
        }

        val hoarding =
          if (possibleRobots == ifWaiting && possibleRobots.nonEmpty) Seq()
          else Seq(Robots(0, 0, 0, 0) -> Minerals(0, 0, 0, 0))

        (hoarding ++ possibleRobots)
          .map { case (robot, minerals) =>
            Sim(timeLeft - 1, blueprint, warehouse.remove(minerals).add(extracted), robots.add(robot))
          }
    }

    runSim(newSims.distinct)
  }

// part 1 (~25 sec)
blueprints
  .map(Sim(24, _, Minerals(0, 0, 0, 0), Robots(1, 0, 0, 0)))
  .map(sim => sim.blueprint.id * runSim(Seq(sim)))
  .sum

// part 2 (very slow, run as App not in worksheet ~5min)
blueprints
  .take(3)
  .map(Sim(32, _, Minerals(0, 0, 0, 0), Robots(1, 0, 0, 0)))
  .map(sim => runSim(Seq(sim)))
  .product
