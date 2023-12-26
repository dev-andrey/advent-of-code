import org.jgrapht.*
import org.jgrapht.alg.*
import org.jgrapht.graph.*

val input = scala.io.Source.fromResource(s"advent2023/day25.txt").getLines().toSeq

val graph = new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])

input.foreach { case s"${comp}: ${connected}" =>
  graph.addVertex(comp)
  connected
    .split(" ")
    .foreach(vertex => graph.addVertex(vertex))
}

input
  .foreach { case s"${comp}: ${connected}" =>
    connected
      .split(" ")
      .foreach { vertex =>
        graph.addEdge(comp, vertex)
      }
  }

val disconnectedSize = new StoerWagnerMinimumCut(graph).minCut().size()
(graph.vertexSet().size() - disconnectedSize) * disconnectedSize
