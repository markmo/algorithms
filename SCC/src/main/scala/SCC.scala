import scala.io.Source

/**
 * Created by markmo on 3/06/2014.
 */
object SCC extends App {

  type Graph = Map[Int, List[Int]]

  type E = List[(Int, Int)]

  type F = Map[Int, Int]

  def findSCC(g: Graph): (F, F) = {

    var t = 0     // number nodes processed so far

    var s = 0     // current source vertex

    val visited = collection.mutable.Set[Int]()

    val f = collection.mutable.Map[Int, Int]()

    val leader = collection.mutable.Map[Int, Int]()

    def DFS(i: Int): Unit = {
      visited += i
      leader(i) = s
      if (g contains i) {
        for (j <- g(i) if !(visited contains j)) {
          DFS(j)
        }
      }
      t += 1
      f(i) = t
    }

    val n = g.keys.max

    for (i <- n to 1 by -1 if !(visited contains i)) {
      s = i
      DFS(i)
    }

    (leader.toMap, f.toMap)

  }

  def reorderVertices(g: Graph, f: F): Graph = {
    val edges = g.toList flatMap {
      case (u, vs) => vs.map(v => (f(u), f(v)))
    }
    adjlist(edges)
  }

  def readGraph(filename: String): Graph = {
    val src = Source.fromURL(getClass.getClassLoader.getResource(filename))
    val edges = src.getLines().map(_ split "\\s+" match {
      case Array(u, v) => (u.toInt, v.toInt)
    }).toList
    adjlist(edges)
  }

  def adjlist(edges: E): Graph = {
    edges groupBy (_._1) map {
      case (u, vs) => (u, vs.map(_._2))
    }
  }

  def reverseArcs(g: Graph): Graph = {
    val edges = g.toList flatMap {
      case (u, vs) => vs.map(v => (v, u))
    }
    adjlist(edges)
  }

  val g = readGraph("SCC.txt")

  val rg = reverseArcs(g)

  // first pass
  val (_, f) = findSCC(rg)

  val g1 = reorderVertices(g, f)

  val (l, _) = findSCC(g1)

  val sccs = l.toList map {
    case (x, y) => (y, x)
  } groupBy (_._1) map {
    case (y, xs) => (y, xs.length)
  }

  val top = sccs.toList
    .sortWith(_._1 > _._1)
    .map(_._2)
    .take(5)

  val ans = for (i <- 0 to 4) yield if (i < top.length) top(i) else 0
  println(ans.mkString(","))

}
