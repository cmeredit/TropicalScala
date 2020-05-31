package testing.Specs

import tropicalcurves.puregraphs._
import tropicalcurves.graphiso._
import org.scalatest._
import Assertions._

class Specs extends FlatSpec with Matchers {

  "A Graph" should "be isomorphic to itself" in {
    val v1 = Vertex(0, Some("v1"))
    val v2 = Vertex(0, Some("v2"))
    val v3 = Vertex(0, Some("v3"))
    val v4 = Vertex(0, Some("v4"))
    val v5 = Vertex(1, Some("v5"))
    val leg1 = new Leg(v5, Some("Leg 1"))
    val leg2 = new Leg(v1, Some("Leg 2"))
    val graph = new UndirectedGraph[Int, Double](Map(
          v1 -> Vector((v1, 1.0), (v2, 1.0), (v4, 1.0)),
          v2 -> Vector((v1, 1.0)),
          v3 -> Vector((v4, 1.0)),
          v4 -> Vector((v3, 1.0), (v1, 1.0)),
          v5 -> Vector()), Set(leg1, leg2))

    assert(GraphIso.graphsAreIsomorphic(graph, graph))
  }

}
