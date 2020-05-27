package tropicalcurves.graphiso
import tropicalcurves.puregraphs._

object GraphIso {

  def graphsAreIsomorphic[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = {
    if (g.spanningForest.size != h.spanningForest.size) false
    else if (g.numVerticesWithCharacteristic != h.numVerticesWithCharacteristic) false
    else checkBruteForceForIsomorphism(g, h)
  }

  private def checkBruteForceForIsomorphism[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = true

}
