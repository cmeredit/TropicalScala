package tropicalcurves.puregraphs

// Type A: Data held by vertices
// Type B: Lengths of edges
class UndirectedGraph[A, B](val adjacency: Map[Vertex[A], Set[(Vertex[A], B)]], val legs: Set[Leg[A]]) {
  val vertices: Set[Vertex[A]] = {
    val keys = adjacency.keySet
    val values = adjacency.toSet.flatMap((kv: (Vertex[A], Set[(Vertex[A], B)])) => {
      kv._2.map(pair => pair._1)
    })
    val roots = legs.flatMap(_.vertices)
    keys ++ values ++ roots
  }
  val numVertices: Int = vertices.size
  val numEdges: Int = adjacency.map(_._2.size).sum
  val numLegs: Int = legs.size

  val verticesByCharacteristic: Map[(Int, Int, A), Set[Vertex[A]]] = {
    val keys: Set[(Int, Int, A)] = vertices.map(v => (edgeDegree(v), legDegree(v), v.data))
    keys.map(k => k -> vertices.filter(v => k == (edgeDegree(v), legDegree(v), v.data))).toMap
  }

  val numVerticesWithCharacteristic: Map[(Int, Int, A), Int] = verticesByCharacteristic.map(kv => kv._1 -> kv._2.size)

  def filterOutVertices(verts: Set[Vertex[A]]): UndirectedGraph[A, B] = {
    val newAdjacency: Map[Vertex[A], Set[(Vertex[A], B)]] = adjacency
      .filterNot(kv => verts.contains(kv._1)) // Filter out verts from the keys
      .map(kv => kv._1 -> kv._2.filterNot(kv => verts.contains(kv._1))) // Filter out verts from the values
    val newLegs: Set[Leg[A]] = legs.filterNot(leg => verts.contains(leg.root)) // Filter out legs with their root in vert
    new UndirectedGraph(newAdjacency, newLegs)
  }

  def filterOutVertex(v: Vertex[A]): UndirectedGraph[A, B] = {
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

  def isConnected: Boolean = spanningForest.size == 1

  def DFS(condition: Vertex[A] => Boolean): Option[Vertex[A]] = {
    spanningForest.foldLeft[Option[Vertex[A]]](None)((currentResult, nextTree) => currentResult match {
      case Some(_) => currentResult
      case None => DFS(condition, nextTree)
    })
  }

  def DFS(condition: Vertex[A] => Boolean, v: Vertex[A]): Option[Vertex[A]] = {
    DFS(condition, getTreeAt(v))
  }

  def DFS(condition: Vertex[A] => Boolean, searchTree: Tree[A]): Option[Vertex[A]] = {
    if (condition(searchTree.root)) Some(searchTree.root)
    else searchTree.connectedForest.foldLeft[Option[Vertex[A]]](None)((currentResult, nextTree) => currentResult match {
      case Some(_) => currentResult
      case None => DFS(condition, nextTree)
    })
  }

  def adjacentVertices(v: Vertex[A]): Set[Vertex[A]] = if (adjacency.keySet.contains(v)) {
    adjacency(v).map(_._1) ++ adjacency.keySet.filter(vert => adjacency(vert).map(_._1).contains(v))
  } else {
    adjacency.keySet.filter(vert => adjacency(vert).map(_._1).contains(v))
  }

  def adjacentDistinctVertices(v: Vertex[A]): Set[Vertex[A]] = adjacentVertices(v) - v

  // Counts the number of endpoints of finite edges at v: Number of things v points to, plus number of things that point
  // to v
  def edgeDegree(v: Vertex[A]): Int = adjacency(v).size + vertices.count(vert => adjacency(vert).map(_._1).contains(v))

  // Counts the number of legs rooted at v
  def legDegree(v: Vertex[A]): Int = legs.count(_.root == v)

  // Counts the endpoints of (possibly infinite) edges at v
  def degree(v: Vertex[A]): Int = edgeDegree(v) + legDegree(v)

  // Counts the number of edges connecting v to w
  def numEdges(v: Vertex[A], w: Vertex[A]): Int = adjacency(v).count(_._1 == w) + adjacency(w).count(_._1 == v)

  override def toString: String = s"Vertices: $vertices\nAdjacency List: $adjacency\nLegs: $legs"
}
