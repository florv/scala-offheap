package jmh

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

import scala.offheap._
import scala.offheap.numeric._

/* In sbt:
   jmh/run DenseMatrix
*/

@State(Scope.Thread)
class DenseMatrix {

  implicit val alloc = malloc

  val offheapMatrix1 = DenseMatrix.rand(100, 100)
  val offheapMatrix2 = DenseMatrix.rand(100, 100)

  val breezeMatrix1 = breeze.linalg.DenseMatrix.rand(100, 100)
  val breezeMatrix2 = breeze.linalg.DenseMatrix.rand(100, 100)

  @Benchmark
  def offheapMultiplication = {
    /* Current issues:
      - Memory is freed only after benchmark. There's a good chance your system will run out of memory
        unless you pass options similar to -f 1 -i 5 -wi 5 when running jmh.
        Surprisingly wrapping the multiplication in a memory region doesn't change anything.
    */
    val res = offheapMatrix1 * offheapMatrix2
    /*
    Region { implicit r =>
      val res = offheapMatrix1 * offheapMatrix2
    }
    */
  }

  @Benchmark
  def breezeMultiplication = {
    val res = breezeMatrix1 * breezeMatrix2
  }
}
