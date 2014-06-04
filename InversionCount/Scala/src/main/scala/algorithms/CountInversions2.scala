package algorithms

import scala.io.Source
import scala.annotation.tailrec

object CountInversions2 extends App {

  def countInversions(ints:List[Int]): (List[Int], Long) = {
    var count = 0L
    def _countInversions(a:List[Int]):List[Int] = {
      @tailrec
      def countSplitInversions(xs:List[Int], ys:List[Int], acc:List[Int]):Stream[Int] = {
        (xs, ys) match {
          case (Nil, Nil) => acc.toStream
          case (_, Nil) => (acc ++ xs).toStream
          case (Nil, _) => (acc ++ ys).toStream
          case (x :: xs1, y :: ys1) =>
            if (x < y) {
              countSplitInversions(xs1, ys, acc :+ x)
            } else {
              count += xs.length
              countSplitInversions(xs, ys1, acc :+ y)
            }
        }
      }
      val n = a.length / 2
      if (n == 0) a
      else {
        val (left, right) = a splitAt n
        countSplitInversions(_countInversions(left), _countInversions(right), List()).toList
      }
    }
    val s = _countInversions(ints)
    (s, count)
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