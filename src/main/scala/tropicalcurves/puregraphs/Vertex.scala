package tropicalcurves.puregraphs

// Simple data holder where equality is based on reference
class Vertex[A](val data: A, val name: Option[String] = None) {
  override def toString: String = name match {
    case Some(str) => s"($str: $data)"
    case None => s"Vertex($data)"
  }
}

object Vertex{
  // Can initialize like a case class
  def apply[A](data: A): Vertex[A] = new Vertex(data)
  def apply[A](data: A, name: Option[String]) = new Vertex[A](data, name)

  // Can pattern match like a case class
  def unapply[A](arg: Vertex[A]): Option[(A, Option[String])] = Some(arg.data, arg.name)
}