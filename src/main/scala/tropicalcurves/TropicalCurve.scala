package tropicalcurves

import tropicalcurves.puregraphs._

class TropicalCurve[B](adjacency: Map[Vertex[Int], Vector[(Vertex[Int], B)]], legs: Set[Leg[Int]]) extends UndirectedGraph[Int, B](adjacency, legs) {

  val core: TropicalCurve[B] = {
    val leavesToPrune = vertices.filter(degree(_) < 2).filter(_.data == 0)
    if (leavesToPrune.isEmpty && legs.isEmpty) {
      this
    } else {
      val graphWithPrunedLeaves = filterOutVertices(leavesToPrune)
      new TropicalCurve[B](graphWithPrunedLeaves.adjacency, Set())
    }
  }

  lazy val bettiNumber: Int = numEdges - numVertices + 1
  lazy val genus: Int = bettiNumber + vertices.map(_.data).sum

}

object TropicalCurve {
  implicit def realizeAsCurve[B](g: UndirectedGraph[Int, B]): TropicalCurve[B] = {
    new TropicalCurve[B](g.adjacency, g.legs)
  }
}