package tropicalcurves.graphiso
import tropicalcurves.puregraphs._

object GraphIso {

  def graphsAreIsomorphic[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = {
    if (g.spanningForest.size != h.spanningForest.size) false
    else if (g.numVerticesWithCharacteristic != h.numVerticesWithCharacteristic) false
    else checkBruteForceForIsomorphism(g, h)
  }

  def getBijections[A, B](m: Map[A, Vector[Vector[B]]]): Vector[Map[A, Vector[B]]] = {
    if (m.isEmpty) Vector()
    else if (m.size == 1) {
      val h = m.head
      val key: A = h._1
      val values: Vector[Vector[B]] = h._2
      values.map(v => Map(key -> v))
    } else {
      val tailBijections = getBijections(m.tail)
      val headBijections = getBijections(Map(m.head))
      for (headBij <- headBijections;
           tailBij <- tailBijections)
        yield headBij ++ tailBij
    }
  }

  private def checkBruteForceForIsomorphism[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = true

}
