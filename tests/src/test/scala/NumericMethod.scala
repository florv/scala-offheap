package test

import org.scalatest.FunSuite
import scala.offheap._
import scala.offheap.numeric._
import scala.offheap.internal.macros._

class NumericMethodSuite extends FunSuite {
  implicit val alloc = malloc

  val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
  import universe._

  test("matrix multiplication") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    opt {
      m * m
    }
  }

  test("matrix multiplication with scalar") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    opt {
      m * 2
    }
  }

  test("2 * A * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      2 * m1 * m2
    }
    assert(res == DenseMatrix(List(List(38, 44), List(86, 100))), "\n" + res.toAscii)
  }

  test("2 * A * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      2 * m1 * m2.t
    }
    assert(res == DenseMatrix(List(List(34, 46), List(78, 106))), "\n" + res.toAscii)
  }

  test("2 * A' * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      2 * m1.t * m2
    }
    assert(res == DenseMatrix(List(List(52, 60), List(76, 88))), "\n" + res.toAscii)
  }

  test("2 * A' * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      2 * m1.t * m2.t
    }
    assert(res == DenseMatrix(List(List(46, 62), List(68, 92))), "\n" + res.toAscii)
  }

  test("A * 2 * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      m1 * (2 * m2)
    }
    assert(res == DenseMatrix(List(List(38, 44), List(86, 100))), "\n" + res.toAscii)
  }

  test("A * 2 * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      m1 * (2 * m2.t)
    }
    assert(res == DenseMatrix(List(List(34, 46), List(78, 106))), "\n" + res.toAscii)
  }

  test("A' * 2 * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      m1.t * (2 * m2)
    }
    assert(res == DenseMatrix(List(List(52, 60), List(76, 88))), "\n" + res.toAscii)
  }

  test("A' * 2 * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      m1.t * (2 * m2.t)
    }
    assert(res == DenseMatrix(List(List(46, 62), List(68, 92))), "\n" + res.toAscii)
  }

  test("A' + B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt {
      m1.t + m2
    }
    assert(res == DenseMatrix(List(List(6, 9), List(9, 12))), "\n" + res.toAscii)
  }

  test("(v * A') * B' + C") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val m3 = DenseMatrix(List(List(9, 10), List(11, 12)))
    val res = opt {
      (2 * m1.t) * m2.t + m3
      m3 * m2
    }
    assert(res == DenseMatrix(List(List(55, 72), List(79, 104))), "\n" + res.toAscii)
  }
}
