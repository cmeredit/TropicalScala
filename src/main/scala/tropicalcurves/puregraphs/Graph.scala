package tropicalcurves.puregraphs

// Type A: Data held by vertices
// Type B: Lengths of edges
class Graph[A, B](val adjacency: Map[Vertex[A], Set[(Vertex[A], B)]], val legs: Set[Leg[A]]) {
  val vertices: Set[Vertex[A]] = adjacency.keySet ++ legs.flatMap(_.vertices)
  val numEdges: Int = adjacency.map(_._2.size).sum
  val numLegs: Int = legs.size

  def filterOutVertices(verts: Set[Vertex[A]]): Graph[A, B] = {
    val newAdjacency: Map[Vertex[A], Set[(Vertex[A], B)]] = adjacency
      .filterNot(kv => verts.contains(kv._1)) // Filter out verts from the keys
      .map(kv => kv._1 -> kv._2.filterNot(kv => verts.contains(kv._1))) // Filter out verts from the values
    val newLegs: Set[Leg[A]] = legs.filterNot(leg => verts.contains(leg.root)) // Filter out legs with their root in vert
    new Graph(newAdjacency, newLegs)
  }

  def filterOutVertex(v: Vertex[A]): Graph[A, B] = {
    filterOutVertices(Set(v))
  }

  def getTreeAt(v: Vertex[A]): Tree[A] = if (vertices.contains(v)) {
    val connectedForest: Set[Tree[A]] = adjacentDistinctVertices(v).foldLeft(Set[Tree[A]]())((currentForest, nextVertex) => {

      val usedVertices: Set[Vertex[A]] = currentForest.flatMap(_.vertices)

      if (usedVertices.contains(nextVertex))
        currentForest
      else {
        currentForest + filterOutVertices(usedVertices + v).getTreeAt(nextVertex)
      }

    })
    new Tree(v, connectedForest)
  } else new Tree(v, Set())

  val spanningForest: Set[Tree[A]] = vertices.foldLeft(Set[Tree[A]]())((currentForest, nextVertex) => {
    val usedVertices = currentForest.flatMap(_.vertices)
    if (usedVertices.contains(nextVertex)) {
      currentForest
    }
    else {
      currentForest + getTreeAt(nextVertex)
    }
  })

  def adjacentVertices(v: Vertex[A]): Set[Vertex[A]] = if (adjacency.keySet.contains(v)) {
    adjacency(v).map(_._1)
  } else {
    Set()
  }

  def adjacentDistinctVertices(v: Vertex[A]): Set[Vertex[A]] = adjacentVertices(v) - v

  // Counts the number of endpoints of finite edges at v
  def edgeDegree(v: Vertex[A]): Int = adjacency(v).size + adjacency(v).count(_._1 == v)

  // Counts the number of legs rooted at v
  def legDegree(v: Vertex[A]): Int = legs.count(_.root == v)

  // Counts the endpoints of (possibly infinite) edges at v
  def degree(v: Vertex[A]): Int = edgeDegree(v) + legDegree(v)

  override def toString: String = s"Vertices: $vertices\nAdjacency List: $adjacency\nLegs: $legs"
}
