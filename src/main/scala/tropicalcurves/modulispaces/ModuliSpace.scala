package tropicalcurves.modulispaces

import tropicalcurves.graphiso._
import tropicalcurves.puregraphs._

class ModuliSpace(val g: Int, val n: Int) {

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

        val mappedAdjacency: Map[Vertex[Int], Set[(Vertex[Int], Double)]] = g.adjacency.map(kv => {
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

  def getSplittingSpecialization(g: UndirectedGraph[Int, Double], v: Vertex[Int], g1: Int, g2: Int,
                                 S: Set[Vertex[Int]], T: Set[Vertex[Int]]): Option[UndirectedGraph[Int, Double]] = {
    val canSplit = g.vertices.contains(v) && // g must have v as a vertex
    g.adjacentVertices(v) == S ++ T && // S and T must cover the adjacent ????? of v
    S.intersect(T).isEmpty &&
    g1 >= 0 && g2 >=0 && v.data == g1 + g2 &&
    {if (g1 == 0) S.size >= 2 else true} &&
    {if (g2 == 0) T.size >= 2 else true}

    None


  }

}
