package tropicalcurves.puregraphs

class Leg[A](val root: Vertex[A], val name: Option[String] = None) {
  def vertices: Vector[Vertex[A]] = Vector(root)

  override def toString: String = name match {
    case Some(str) => s"($str, $root)"
    case None => s"Leg($root)"
  }
}
