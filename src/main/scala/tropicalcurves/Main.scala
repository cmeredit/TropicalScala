package tropicalcurves

import tropicalcurves.graphiso.GraphIso
import tropicalcurves.puregraphs._

import scala.concurrent.Await
import scala.concurrent.duration._

object Main extends App {

  val v = Vertex(0, Some("Joe"))
  val v1 = Vertex(0, Some("v1"))
  val v2 = Vertex(0, Some("v2"))
  val v3 = Vertex(0, Some("v3"))
  val v4 = Vertex(0, Some("v4"))
  val e = new Edge(v, v)
  val leg = new Leg(v)

  val g1 = new Graph[Int, Double](Map(
    v -> Set((v1, 1.0), (v4, 1.0)),
    v1 -> Set((v, 1.0), (v2, 1.0), (v4, 1.0)),
    v2 -> Set((v1, 1.0)),
    v3 -> Set(),
    v4 -> Set((v, 1.0), (v1, 1.0))),
    Set(leg))

  g1.spanningForest foreach println
  println("G is " + {
    if (g1.isConnected) "" else "not "
  } + "connected!")

  println(GraphIso.graphsAreIsomorphic(g1, g1))
  println(GraphIso.graphsAreIsomorphic(g1.filterOutVertex(v), g1))

  println(GraphIso.getBijections[Int, Int](Map(0 -> Vector(Vector(0, 1), Vector(1, 0)), 1 -> Vector(Vector(2, 3), Vector(3, 2)))))

  val myFutureGraphs = GraphIso.assimilateNewGraphs(Vector(g1), Vector(g1.filterOutVertex(v), g1, g1, g1, g1))

  println("Reducing graphs!!!")
  val reducedGraphs = Await.result(myFutureGraphs, 3.seconds)
  reducedGraphs.foreach(g => {
    println(g)
    println("Next graph...")
  })
}
