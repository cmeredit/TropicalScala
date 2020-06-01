package tropicalcurves.modulispaces

import tropicalcurves.graphiso._
import tropicalcurves.puregraphs._

class ModuliSpace(val g: Int, val n: Int) {

  object Specialization {
    def getGenusSpecialization(g: UndirectedGraph[Int, Double], v: Vertex[Int]): Option[UndirectedGraph[Int, Double]] = {
      if (!g.vertices.contains(v) || v.data < 1) None
      else {
        if (v.data > 1 || g.degree(v) > 0) Some({

          val newName = v.name.map(name => s"(G.R. of $name)")
          val newVert: Vertex[Int] = Vertex(v.data - 1, newName)

          val newLegs: Set[Leg[Int]] = g.legs.map {leg => {
            if (leg.root == v) new Leg(newVert, leg.name)
            else leg
          }}

          val mappedAdjacency: Map[Vertex[Int], Vector[(Vertex[Int], Double)]] = g.adjacency.map(kv => {
            val (vert, adj) = kv
            val newKey = if (vert == v) newVert else vert
            val newVal = adj.map(vl => {
              val (adjVert, length) = vl
              if (adjVert == v) (newVert, length) else (adjVert, length)
            })
            (newKey, newVal)
          })

          val newAdjacency = mappedAdjacency.map( kv => {
            val (vert, adj) = kv
            if (vert == newVert) {
              (vert, adj ++ Set((newVert, 1.0)))
            } else kv
          })

          new UndirectedGraph(newAdjacency, newLegs)
        })
        else None
      }
    }

    def partitionAdjacency(adj: Map[Vertex[Int], Vector[(Vertex[Int], Double)]],
                                   baseVert: Vertex[Int],
                                   newVert1: Vertex[Int],
                                   newVert2: Vertex[Int]): Vector[Map[Vertex[Int], Vector[(Vertex[Int], Double)]]] = {
      type AdjMap = Map[Vertex[Int], Vector[(Vertex[Int], Double)]]
      // First handle the case where baseVert points to other vertices
      if (adj.keySet.contains(baseVert) && adj(baseVert).nonEmpty) {
        val vertToFlip = adj(baseVert).head
        val updatedAdj: AdjMap = adj.transform((k, v) => if (k == baseVert) v.drop(1) else v)

        val newAdj1: AdjMap = if (updatedAdj.keySet.contains(newVert1)) {
          updatedAdj.transform((k, v) => if (k == newVert1) v.appended(vertToFlip) else v)
        } else updatedAdj ++ Map(newVert1 -> Vector(vertToFlip))

        val newAdj2: AdjMap = if (updatedAdj.keySet.contains(newVert2)) {
          updatedAdj.transform((k, v) => if (k == newVert2) v.appended(vertToFlip) else v)
        } else updatedAdj ++ Map(newVert2 -> Vector(vertToFlip))

        println(s"First case original adj: $adj")
        println(s"First case updated adj: $updatedAdj")
        println(s"New adj 1: $newAdj1")
        println(s"New adj 2: $newAdj2")

        partitionAdjacency(newAdj1, baseVert, newVert1, newVert2) ++ partitionAdjacency(newAdj2, baseVert, newVert1, newVert2)
      }
      // Then handle the case where baseVert is pointed to
      else if (adj.toSet.flatMap((x: (Vertex[Int], Vector[(Vertex[Int], Double)])) => x._2).map(_._1).contains(baseVert)) {

        def onceRemoved(m: AdjMap, elt: Vertex[Int]): Option[(AdjMap, Vertex[Int], (Vertex[Int], Double))] = {
          adj.find(_._2.map(_._1).contains(elt)) match {
            case Some((keyToUpdate, valToUpdate)) => Some({
              val adjacentEdges = m(keyToUpdate)
              val edgeToUpdate = adjacentEdges.find(_._1 == elt).get
              val indexToUpdate = adjacentEdges.indexOf(edgeToUpdate)

              def removedAtIndex[A](vec: Vector[A], index: Int): Vector[A] = {
                vec.zipWithIndex.filter(_._2 != index).map(_._1)
              }

              (m.transform((k, v) => {
                if (k == keyToUpdate) removedAtIndex(valToUpdate, indexToUpdate)
                else v
              }), keyToUpdate, edgeToUpdate)
            })
            case None => None
          }
        }

        val (updatedAdj, sourceVertex, removedEdge): (AdjMap, Vertex[Int], (Vertex[Int], Double)) = onceRemoved(adj, baseVert).get
        println(s"\nRemoving $baseVert once from $adj")
        println(s"Got updated adj: $updatedAdj\nGot source of removal: $sourceVertex\nGot removed edge: $removedEdge\n")

        val replacementEdge1 = (newVert1, removedEdge._2)
        val replacementEdge2 = (newVert2, removedEdge._2)

        val newAdj1: AdjMap = updatedAdj.transform((k, v) => if (k == sourceVertex) v.appended(replacementEdge1) else v)

        val newAdj2: AdjMap = updatedAdj.transform((k, v) => if (k == sourceVertex) v.appended(replacementEdge2) else v)

        partitionAdjacency(newAdj1, baseVert, newVert1, newVert2) ++ partitionAdjacency(newAdj2, baseVert, newVert1, newVert2)
      }
      // And finally, handle the case where baseVert isn't pointed to or from
      else {
//        println(s"$baseVert wasn't pointed to or from")
        Vector(adj)
      }
    }

    def getSplittingSpecialization(g: UndirectedGraph[Int, Double], v: Vertex[Int], g1: Int, g2: Int,
                                   S: Set[Vertex[Int]], T: Set[Vertex[Int]]): Option[UndirectedGraph[Int, Double]] = {
      val canSplit = g.vertices.contains(v) && // g must have v as a vertex
        g.adjacentVertices(v).toSet == S ++ T && // S and T must cover the adjacent ????? of v
        S.intersect(T).isEmpty &&
        g1 >= 0 && g2 >=0 && v.data == g1 + g2 &&
        {if (g1 == 0) S.size >= 2 else true} &&
        {if (g2 == 0) T.size >= 2 else true}

      None


    }
  }
}
