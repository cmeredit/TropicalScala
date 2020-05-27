package tropicalcurves.graphiso
import tropicalcurves.puregraphs._

object GraphIso {

  def graphsAreIsomorphic[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = {
    if (g.numVertices != h.numVertices) false
    else if (g.numEdges != h.numEdges) false
    else if (g.numLegs != h.numLegs) false
    else checkBruteForceForIsomorphism(g, h)
  }

  private def checkBruteForceForIsomorphism[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = ???

}
