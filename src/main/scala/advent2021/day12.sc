import scala.annotation.tailrec

val raw = scala.io.Source.fromResource(s"advent2021/day12.txt").getLines()

val input =
  raw.map(_.split('-')).flatMap(arr => Vector(arr.head -> arr.last, arr.last -> arr.head)).toVector.groupMap(_._1)(_._2)
val start = "start"
val end   = "end"

// part 1

Vector(start).flatMap(node => input(node).map(next => Vector(node, next)))

final case class Problem(path: Vector[String], visited: Set[String])

@tailrec
def findPaths(solutions: Vector[Problem]): Vector[Problem] = {
  val (solved, unsolved) = solutions.partition(_.path.last == end)
  if (unsolved.isEmpty) solved
  else {
    val nextStep = unsolved.flatMap(prob =>
      input(prob.path.last)
        .filterNot(prob.visited)
        .map(next => Problem(prob.path :+ next, prob.visited ++ Set(next).filter(_.head.isLower)))
    )
    findPaths(solved ++ nextStep)
  }
}

findPaths(Vector(Problem(Vector(start), Set(start)))).size

// part 2
final case class CaveSystem(path: Vector[String], visited: Set[String], visitedTwice: Set[String])

@tailrec
def findPaths2(solutions: Vector[CaveSystem]): Vector[CaveSystem] = {
  val (solved, unsolved) = solutions.partition(_.path.last == end)
  if (unsolved.isEmpty) solved
  else {
    val nextStep = unsolved.flatMap(prob =>
      input(prob.path.last)
        .filterNot(_ == start)
        .flatMap { nextCave =>
          if (nextCave.head.isUpper)
            Vector(CaveSystem(prob.path :+ nextCave, prob.visited ++ Set(nextCave), prob.visitedTwice))
          else if (prob.visited.contains(nextCave) && prob.visitedTwice.isEmpty)
            Vector(CaveSystem(prob.path :+ nextCave, prob.visited + nextCave, Set(nextCave)))
          else if (!prob.visited.contains(nextCave))
            Vector(CaveSystem(prob.path :+ nextCave, prob.visited + nextCave, prob.visitedTwice))
          else
            Vector()
        }
    )
    findPaths2(solved ++ nextStep)
  }
}

findPaths2(Vector(CaveSystem(Vector(start), Set(start), Set()))).size
