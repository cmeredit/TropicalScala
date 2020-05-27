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

  private def conditionHoldsForAll[A](collection: Iterable[A])(condition: A => Boolean): Boolean = {
    collection.foldLeft(true)((holdsSoFar, nextItem) => {
      if (holdsSoFar) condition(nextItem)
      else false
    })
  }

  def checkIfBijectionPreservesEdges[A, B](domain: Graph[A, B], codomain: Graph[A, B],
                                           bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    val domainVertexPairs: Set[(Vertex[A], Vertex[A])] = for (v <- domain.vertices;
                                                              w <- domain.vertices) yield (v, w)
    conditionHoldsForAll(domainVertexPairs)(vertexPair => {
      val (v, w) = vertexPair
      val numInputEdges: Int = domain.adjacency(v).count(_._1 == w)
      val numOutputEdges: Int = codomain.adjacency(bij(v)).count(_._1 == bij(w))
      numInputEdges == numOutputEdges
    })
  }

  def checkIfBijectionPreservesData[A, B](domain: Graph[A, B], bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    conditionHoldsForAll(domain.vertices)(v => v.data == bij(v).data)
  }

  def checkIfBijectionPreservesLegs[A, B](domain: Graph[A, B], codomain: Graph[A, B],
                                          bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    conditionHoldsForAll(domain.vertices)(v => domain.legDegree(v) == codomain.legDegree(bij(v)))
  }

  def checkIfBijectionIsIsomorphism[A, B](domain: Graph[A, B], codomain: Graph[A, B],
                                          bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    checkIfBijectionPreservesEdges(domain, codomain, bij) &&
      checkIfBijectionPreservesData(domain, bij) &&
      checkIfBijectionPreservesLegs(domain, codomain, bij)
  }

  private def checkBruteForceForIsomorphism[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = true

}
