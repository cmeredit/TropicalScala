package tropicalcurves.puregraphs

class Graph[A](val manualVertices: Set[Vertex[A]], val edges: Set[Edge[A]], val legs: Set[Leg[A]]) {
  val vertices: Set[Vertex[A]] = manualVertices ++ edges.flatMap(_.vertices) ++ legs.flatMap(_.vertices)

  override def toString: String = s"Vertices: $manualVertices\nEdges: $edges\nLegs: $legs"
}
