package algorithms

import scala.io.Source
import scala.annotation.tailrec

object CountInversions extends App {

  def countInversions(a:List[Int]):(List[Int], Long) = countInversions(a, a.length)

  def countInversions(a:List[Int], n:Int): (List[Int], Long) = {
    if (n == 1) {
      (a, 0)
    } else {
      val l = n / 2
      val m = n - l
      val (b, c) = a.splitAt(l)
      val (sx, x) = countInversions(b, l)
      val (sy, y) = countInversions(c, m)
      val (sz, z) = countSplitInversions(sx, sy, (List(), 0))
      (sz, x + y + z)
    }
  }

  @tailrec
  def countSplitInversions(xs:List[Int], ys:List[Int], acc:(List[Int], Long)):(List[Int], Long) = {
    (xs, ys) match {
      case (Nil, Nil) => acc
      case (_, Nil) => (acc._1 ++ xs, acc._2)
      case (Nil, _) => (acc._1 ++ ys, acc._2)
      case (x :: xs1, y :: ys1) =>
        if (x <= y) {
          countSplitInversions(xs1, ys, (acc._1 :+ x, acc._2))
        } else {
          countSplitInversions(xs, ys1, (acc._1 :+ y, acc._2 + xs.length))
        }
    }
  }

  def testCountInversions() = {
    val (s1, x1) = countInversions(List(6, 5, 4, 3, 2, 1))
    println(s1.mkString(","))
    println(x1)
    val (s2, x2) = countInversions(List(4, 80, 70, 23, 9, 60, 68, 27, 66, 78, 12, 40, 52, 53, 44, 8, 49, 28, 18, 46, 21, 39, 51, 7, 87, 99, 69, 62, 84, 6, 79, 67, 14, 98, 83, 0, 96, 5, 82, 10, 26, 48, 3, 2, 15, 92, 11, 55, 63, 97, 43, 45, 81, 42, 95, 20, 25, 74, 24, 72, 91, 35, 86, 19, 75, 58, 71, 47, 76, 59, 64, 93, 17, 50, 56, 94, 90, 89, 32, 37, 34, 65, 1, 73, 41, 36, 57, 77, 30, 22, 13, 29, 38, 16, 88, 61, 31, 85, 33, 54))
    println(s2.mkString(","))
    println(x2)
  }

  override def main(args: Array[String]) {
    testCountInversions()
    val lines = Source.fromURL(getClass.getClassLoader.getResource("IntegerArray.txt")).getLines()
    val integers = lines.map(_.toInt).toList
    val start = System.currentTimeMillis()
    val (s, x) = countInversions(integers)
    val end = System.currentTimeMillis()
    println(s.take(100).mkString(","))
    println(x)
    println("Execution time (ms): " + (end - start))
  }

}