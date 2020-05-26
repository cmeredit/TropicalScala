package tropicalcurves.puregraphs

class Graph[A](val manualVertices: Set[Vertex[A]], val edges: Set[Edge[A]], val legs: Set[Leg[A]]) {
  val vertices: Set[Vertex[A]] = manualVertices ++ edges.flatMap(_.vertices) ++ legs.flatMap(_.vertices)

  // Counts the number of endpoints of finite edges at v
  def edgeDegree(v: Vertex[A]): Int = edges.count(_._1 == v) + edges.count(_._2 == v)

  // Counts the number of legs rooted at v
  def legDegree(v: Vertex[A]): Int = legs.count(_.root == v)

  // Counts the endpoints of (possibly infinite) edges at v
  def degree(v: Vertex[A]): Int = edgeDegree(v) + legDegree(v)

  override def toString: String = s"Vertices: $manualVertices\nEdges: $edges\nLegs: $legs"
}
