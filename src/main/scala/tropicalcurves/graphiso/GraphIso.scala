package tropicalcurves.graphiso

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout

import scala.concurrent.duration._
import tropicalcurves.puregraphs._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Success


object GraphIso {


  implicit val isoSupervisor: ActorSystem[IsoMessage] = ActorSystem(IsomorphismCheckSupervisor(), "IsoSupervisor")
  implicit val ec: ExecutionContextExecutor = isoSupervisor.executionContext
  implicit val timeout: Timeout = Timeout(5.seconds)

  def makeSupervisorMimic(message: String): Unit = {

//    val result: Future[IsoMessage] = isoSupervisor.ask((ref: ActorRef[IsoMessage]) => Mimic(message, ref))
//    result.onComplete {
//      case Success(Mimic2(msg)) => println(s"Received back $msg")
//      case _ => println("Noooooooooo!!!!!!!")
//    }
  }

  def printGraphs[A, B](graphs: Vector[Graph[A, B]]): Unit = {
    isoSupervisor ! ReduceGraphsByIsomorphism(graphs)
  }

  def shutdownSystem(): Unit = isoSupervisor.terminate()


  // Type A should be the characteristic of a vertex
  // Type B should be Vertex
  def getBijections[A, B](m: Map[A, Vector[Vector[B]]]): Vector[Map[A, Vector[B]]] = {
    if (m.isEmpty) Vector()
    else if (m.size == 1) {
      val h = m.head
      val key: A = h._1
      val values: Vector[Vector[B]] = h._2
      values.map(v => Map(key -> v))
    } else {
      val tailBijections = getBijections(m.tail)
      val headBijections = getBijections(Map(m.head))
      for (headBij <- headBijections;
           tailBij <- tailBijections)
        yield headBij ++ tailBij
    }
  }

  def getBijection[A, B](inputs: Vector[A], outputs: Vector[B]): Option[Map[A, B]] = {
    if (inputs.size != outputs.size) None
    else Some(inputs.indices.map(i => inputs(i) -> outputs(i)).toMap)
  }

  private def getBijections[A, B](domain: Graph[A, B], codomain: Graph[A, B]): Option[Vector[Map[Vertex[A], Vertex[A]]]] = {
    if (domain.numVertices != codomain.numVertices) None
    else {
      val inputs: Vector[Vertex[A]] = domain.vertices.toVector
      val allOutputs: Vector[Vector[Vertex[A]]] = codomain.vertices.toVector.permutations.toVector
      Some(allOutputs.flatMap(op => getBijection(inputs, op)))
    }
  }



  private def conditionHoldsForAll[A](collection: Iterable[A])(condition: A => Boolean): Boolean = {
    collection.foldLeft(true)((holdsSoFar, nextItem) => {
      if (holdsSoFar) condition(nextItem)
      else false
    })
  }

  private def conditionHoldsForSome[A](collection: Iterable[A])(condition: A => Boolean): Boolean = {
    collection.foldLeft(false)((hasHeld, nextItem) => {
      if (hasHeld) true
      else condition(nextItem)
    })
  }

  def checkIfBijectionPreservesEdges[A, B](domain: Graph[A, B], codomain: Graph[A, B],
                                           bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    val domainVertexPairs: Set[(Vertex[A], Vertex[A])] = for (v <- domain.vertices;
                                                              w <- domain.vertices) yield (v, w)
    conditionHoldsForAll(domainVertexPairs)(vertexPair => {
      val (v, w) = vertexPair
      domain.numEdges(v, w) == codomain.numEdges(bij(v), bij(w))
    })
  }

  def checkIfBijectionPreservesData[A, B](domain: Graph[A, B], bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    conditionHoldsForAll(domain.vertices)(v => v.data == bij(v).data)
  }

  def checkIfBijectionPreservesLegs[A, B](domain: Graph[A, B], codomain: Graph[A, B],
                                          bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    conditionHoldsForAll(domain.vertices)(v => domain.legDegree(v) == codomain.legDegree(bij(v)))
  }

  def checkIfBijectionIsIsomorphism[A, B](domain: Graph[A, B], codomain: Graph[A, B],
                                          bij: Map[Vertex[A], Vertex[A]]): Boolean = {
    checkIfBijectionPreservesEdges(domain, codomain, bij) &&
      checkIfBijectionPreservesData(domain, bij) &&
      checkIfBijectionPreservesLegs(domain, codomain, bij)
  }



  def graphsAreIsomorphic[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = {
    if (g.spanningForest.size != h.spanningForest.size) false // Number of connected components
    else if (g.numVerticesWithCharacteristic != h.numVerticesWithCharacteristic) false // Counts of vertices by char.
    else checkBruteForceForIsomorphism(g, h) // Easy tests failed - have to brute force
  }

  private def checkBruteForceForIsomorphism[A, B](g: Graph[A, B], h: Graph[A, B]): Boolean = {
    getBijections[A, B](g, h) match {
      case None => false
      case Some(v) => conditionHoldsForSome(v)(bij => checkIfBijectionIsIsomorphism(g, h, bij))
    }
  }

  def reduceGraphsByIsomorphism[A, B](graphs: Vector[Graph[A, B]]): Vector[Graph[A, B]] = {
    graphs.foldLeft(Vector[Graph[A, B]]())((currentIsotypes, nextGraph) => {
      val graphIsNew = conditionHoldsForAll(currentIsotypes)(isotype => !graphsAreIsomorphic(isotype, nextGraph))
      
      if (graphIsNew) currentIsotypes.appended(nextGraph)
      else currentIsotypes
    })
  }

  // Does NOT reduce uniqueGraphs by isomorphism
  // Only adds a graph from newGraphs to uniqueGraphs if it is not already present in uniqueGraphs up to isomorphism
  def assimilateNewGraphs[A, B](uniqueGraphs: Vector[Graph[A, B]], newGraphs: Vector[Graph[A, B]]): Vector[Graph[A, B]] = {
    val newGraphsReduced: Vector[Graph[A, B]] = reduceGraphsByIsomorphism(newGraphs)
    val newGraphsFiltered: Vector[Graph[A, B]] = newGraphsReduced.filter(h => {
      conditionHoldsForAll(uniqueGraphs)(g => !graphsAreIsomorphic(g, h))
    })
    uniqueGraphs ++ newGraphsFiltered
  }

}
