package tropicalcurves.puregraphs

// Simple data holder where equality is based on reference
class Vertex[A](val data: A)

object Vertex{
  // Can initialize like a case class
  def apply[A](data: A): Vertex[A] = new Vertex(data)

  // Can pattern match like a case class
  def unapply[A](arg: Vertex[A]): Option[A] = Some(arg.data)
}