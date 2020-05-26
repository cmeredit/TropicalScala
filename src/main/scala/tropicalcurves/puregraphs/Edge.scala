package tropicalcurves.puregraphs

class Edge[A](val _1: Vertex[A], val _2: Vertex[A], val name: Option[String] = None) {
  def vertices: Vector[Vertex[A]] = Vector(_1, _2)

  override def toString: String = name match {
    case Some(str) => str
    case None => "Edge(" + _1.toString + ", " + _2.toString + ")"
  }

}
