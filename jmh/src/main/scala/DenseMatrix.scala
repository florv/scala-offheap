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

  implicit val props = Region.Props(Pool(pageSize=81920 * 100, chunkSize=81920 * 1000))

  val d1: Double = math.random
  val d2: Double = math.random

  val offheapMatrix1 = DenseMatrix.rand(100, 100)(malloc)
  val offheapMatrix2 = DenseMatrix.rand(100, 100)(malloc)
  val offheapMatrix3 = DenseMatrix.rand(100, 100)(malloc)

  val breezeMatrix1 = breeze.linalg.DenseMatrix.rand(100, 100)
  val breezeMatrix2 = breeze.linalg.DenseMatrix.rand(100, 100)
  val breezeMatrix3 = breeze.linalg.DenseMatrix.rand(100, 100)

  @Benchmark
  def offheapMultiplication = {
    /* Current issues:
      - Memory is freed only after benchmark. There's a good chance your system will run out of memory
        unless you pass options similar to -f 1 -i 5 -wi 5 when running jmh.
        Surprisingly wrapping the multiplication in a memory region doesn't change anything.
    */
    //val res = offheapMatrix1 * offheapMatrix2
    Region { implicit r =>
      offheapMatrix1 * offheapMatrix2
    }
  }

  @Benchmark
  def breezeMultiplication = {
    breezeMatrix1 * breezeMatrix2
  }

  @Benchmark
  def offheapMultiplication2 = {
    Region { implicit r =>
      d1 * offheapMatrix1 * offheapMatrix2 + d2 * offheapMatrix3
    }
  }

  @Benchmark
  def offheapOptimizedMultiplication2 = {
    Region { implicit r =>
      opt {
        d1 * offheapMatrix1 * offheapMatrix2 + d2 * offheapMatrix3
      }
    }
  }

  @Benchmark
  def breezeMultiplication2 = {
    d1 * breezeMatrix1 * breezeMatrix2 + d2 * breezeMatrix3
  }
}
