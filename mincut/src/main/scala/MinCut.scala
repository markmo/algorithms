import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
 * Created by markmo on 31/05/2014.
 */
object MinCut extends App {

  case class Vertex(id: Int, contracted: List[Vertex] = Nil) {

    def contract(b: Vertex) = {
//      println("Contracting " + b.id + " into " + id)
      Vertex(id, Vertex(b.id) :: b.contracted ::: contracted)
    }

    override def equals(o: Any) = o match {
      case v: Vertex => v.id == id
      case _ => false
    }

  }

  case class Edge(a: Vertex, b: Vertex) {

    override def toString = a.id + " " + b.id

    override def hashCode = a.hashCode() + b.hashCode()

    override def equals(o: Any) = o match {
      case e: Edge =>
        (e.a == a && e.b == b) || (e.a == b && e.b == a)
      case _ => false
    }

  }

  case class Graph(V: List[Vertex], E: List[Edge]) {

    def contract(e: Edge) = {
      val v = e.a contract e.b
      val V1 = v :: (V diff List(e.a, e.b))
      val E1 = E map {
        case Edge(a, b) if b == e.a || b == e.b => Edge(a, v)
        case Edge(a, b) if a == e.a || a == e.b => Edge(v, b)
        case edge => edge
      } filter { edge => edge.a != edge.b }
      Graph(V1, E1)
    }

  }

  def mincut(g: Graph, seed: Long = 0): (Int, List[Edge]) = {

    val r = new Random(seed)

    @tailrec
    def contract(g: Graph): Graph = {

//      println("\ng:")
//      g.E foreach { e =>
//        println(e)
//      }

      val ri = r.nextInt(g.E.length)
//      println("ri: " + ri)

      val e = g.E(ri)
//      println("e:")
//      println(e)

      val g1 = g contract e

      if (g1.V.length > 2) {
        contract(g1)
      } else {
        g1
      }
    }

    val g1 = contract(g)
    val v0 = g1.V(0)
    val v1 = g1.V(1)

    // TODO expansion using too much memory
    val c1 = for {
      a <- v0.id :: v0.contracted.map(_.id)
      b <- v1.id :: v1.contracted.map(_.id)
    } yield Edge(Vertex(a), Vertex(b))
    val cuts = g.E intersect c1
    (cuts.length, cuts)
  }

  def readGraph(filename: String) = {
    val src = Source.fromURL(getClass.getClassLoader.getResource(filename))
    val adjlist = src.getLines().map(_ split "\\s+" match {
      case Array(x, xs@_*) => (x.toInt, xs.map(_.toInt))
    }).toList
    val V = for { t <- adjlist } yield Vertex(t._1)
    val Vmap = V.map(v => (v.id, v)).toMap
    val E = for {
      t <- adjlist
      b <- t._2
    } yield if (t._1 < b)
        Edge(Vmap(t._1), Vmap(b))
      else
        Edge(Vmap(b), Vmap(t._1))
    Graph(V, E.distinct)
  }

  //val g = readGraph(args(0))
  val g = readGraph("kargerMinCut.txt")

  var k = g.V.length - 1
  var cs: List[Edge] = Nil

  for (i <- 1 to 500) {
    val ans = mincut(g, System.currentTimeMillis)

    if (ans._1 < k) {
      k = ans._1
      cs = ans._2
    }

//    println(ans._1)

//    ans._2 foreach { e =>
//      println(e)
//    }
  }

  println(k)
  cs foreach { e =>
    println(e)
  }


  //  println("Calculating mincut...")
//  val a = mincut(g)
//  println(a)

}
