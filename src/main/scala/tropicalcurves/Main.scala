package tropicalcurves

import tropicalcurves.graphiso.GraphIso
import tropicalcurves.modulispaces.ModuliSpace
import tropicalcurves.puregraphs._

import scala.concurrent.Await
import scala.concurrent.duration._

object Main extends App {

//  val v = Vertex(1, Some("Joe"))
//  val v1 = Vertex(0, Some("v1"))
//  val v2 = Vertex(0, Some("v2"))
//  val v3 = Vertex(0, Some("v3"))
//  val v4 = Vertex(0, Some("v4"))
//  val e = new Edge(v, v)
//  val leg = new Leg(v)
//
//  val g1 = new Graph[Int, Double](Map(
//    v -> Set((v1, 1.0), (v4, 1.0)),
//    v1 -> Set((v, 1.0), (v2, 1.0), (v4, 1.0)),
//    v2 -> Set((v1, 1.0)),
//    v3 -> Set(),
//    v4 -> Set((v, 1.0), (v1, 1.0))),
//    Set(leg))

  {
    val v1 = Vertex(0, Some("v1"))
    val v2 = Vertex(0, Some("v2"))
    val v3 = Vertex(1, Some("v3"))
    val adjacency = Map(
      v1 -> Vector((v1, 1.0), (v2, 1.0), (v3, 1.0)),
      v2 -> Vector((v3, 1.0)),
      v3 -> Vector[(Vertex[Int], Double)]()
    )
    val legs = Set(new Leg(v1, Some("leg 1")))
    val graph = new UndirectedGraph(adjacency, legs)

    val myModuliSpace = new ModuliSpace(1, 2)
    println(s"My graph:\n$graph")
    println(s"\n\nGR at v3:\n${myModuliSpace.Specialization.getGenusSpecialization(graph, v3)}")

//    myModuliSpace.Specialization.getSplittingSpecializations(graph, v3, 0, 1) match {
//      case Some(vec) => for (g <- vec) {
//        println(s"\n\nCurrent graph: \n$g")
//        println("\nEasy to read adjacency:")
//        g.adjacency foreach println
//      }
//      case None => assert(false)
//    }
//    val newVert1 = Vertex(0, Some("New vertex 1"))
//    val newVert2 = Vertex(0, Some("New vertex 2"))
//    println("\n\nAdjacency partitions: ")
//    myModuliSpace.Specialization.partitionAdjacency(graph.adjacency, v2, newVert1, newVert2).foreach(x => println(x.filter(_._1 != v2)))

    val startTime = System.nanoTime()
    val m12Curves = myModuliSpace.getSpace
    val endTime = System.nanoTime()
    println(s"M-1-2 apparently has ${m12Curves.size} curves!")
    println(s"Generation took ${endTime - startTime} nanoseconds, or ${(endTime - startTime).toDouble / 1_000_000_000.0} seconds!")
    m12Curves.foreach(g => println(s"\n\n$g"))
  }

//  g1.spanningForest foreach println
//  println("G is " + {
//    if (g1.isConnected) "" else "not "
//  } + "connected!")
//
//  println(GraphIso.graphsAreIsomorphic(g1, g1))
//  println(GraphIso.graphsAreIsomorphic(g1.filterOutVertex(v), g1))
//
//  println(GraphIso.getBijections[Int, Int](Map(0 -> Vector(Vector(0, 1), Vector(1, 0)), 1 -> Vector(Vector(2, 3), Vector(3, 2)))))
//
//  val myFutureGraphs = GraphIso.assimilateNewGraphs(Vector(g1), Vector(g1.filterOutVertex(v), g1, g1, g1, g1))
//
//  println("Reducing graphs!!!")
//  val reducedGraphs = Await.result(myFutureGraphs, 3.seconds)
//  reducedGraphs.foreach(g => {
//    println(g)
//    println("Next graph...")
//  })
}
