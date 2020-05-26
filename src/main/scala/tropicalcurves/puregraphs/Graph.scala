package tropicalcurves.puregraphs

// Type A: Data held by vertices
// Type B: Lengths of edges
class Graph[A, B](val adjacency: Map[Vertex[A], Set[(Vertex[A], B)]], val legs: Set[Leg[A]]) {
  val vertices: Set[Vertex[A]] = adjacency.keySet ++ legs.flatMap(_.vertices)
  val numEdges: Int = adjacency.map(_._2.size).sum
  val numLegs: Int = legs.size

  def adjacentVertices(v: Vertex[A]): Set[Vertex[A]] = if (adjacency.keySet.contains(v)) {
    adjacency(v).map(_._1)
  } else {
    Set()
  }

  // Counts the number of endpoints of finite edges at v
  def edgeDegree(v: Vertex[A]): Int = adjacency(v).size + adjacency(v).count(_._1 == v)

  // Counts the number of legs rooted at v
  def legDegree(v: Vertex[A]): Int = legs.count(_.root == v)

  // Counts the endpoints of (possibly infinite) edges at v
  def degree(v: Vertex[A]): Int = edgeDegree(v) + legDegree(v)

  override def toString: String = s"Vertices: $vertices\nAdjacency List: $adjacency\nLegs: $legs"
}
