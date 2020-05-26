package tropicalcurves.puregraphs

class Tree[A](val root: Vertex[A], val connectedForest: Set[Tree[A]]) {
  val vertices: Set[Vertex[A]] = Set(root) ++ connectedForest.flatMap(_.vertices)

  override def toString: String = s"[$root, $connectedForest]"
}
