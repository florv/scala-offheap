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
    opt(alloc, {
      m * m
    })
  }

  test("matrix multiplication with scalar") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    opt(alloc, {
      m * 2
    })
  }

  test("2 * A * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      2 * m1 * m2
    })
    assert(res == DenseMatrix(List(List(38, 44), List(86, 100))), "\n" + res.toAscii)
  }

  test("2 * A * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      2 * m1 * m2.t
    })
    assert(res == DenseMatrix(List(List(34, 46), List(78, 106))), "\n" + res.toAscii)
  }

  test("2 * A' * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      2 * m1.t * m2
    })
    assert(res == DenseMatrix(List(List(52, 60), List(76, 88))), "\n" + res.toAscii)
  }

  test("2 * A' * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      2 * m1.t * m2.t
    })
    assert(res == DenseMatrix(List(List(46, 62), List(68, 92))), "\n" + res.toAscii)
  }

  test("A * 2 * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      m1 * (2 * m2)
    })
    assert(res == DenseMatrix(List(List(38, 44), List(86, 100))), "\n" + res.toAscii)
  }

  test("A * 2 * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      m1 * (2 * m2.t)
    })
    assert(res == DenseMatrix(List(List(34, 46), List(78, 106))), "\n" + res.toAscii)
  }

  test("A' * 2 * B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      m1.t * (2 * m2)
    })
    assert(res == DenseMatrix(List(List(52, 60), List(76, 88))), "\n" + res.toAscii)
  }

  test("A' * 2 * B'") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      m1.t * (2 * m2.t)
    })
    assert(res == DenseMatrix(List(List(46, 62), List(68, 92))), "\n" + res.toAscii)
  }

  test("A' + B") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val res = opt(alloc, {
      m1.t + m2
    })
    assert(res == DenseMatrix(List(List(6, 9), List(9, 12))), "\n" + res.toAscii)
  }

  test("(v * A') * B' + C") {
    val m1 = DenseMatrix(List(List(1, 2), List(3, 4)))
    val m2 = DenseMatrix(List(List(5, 6), List(7, 8)))
    val m3 = DenseMatrix(List(List(9, 10), List(11, 12)))
    val res = opt(alloc, {
      (2 * m1.t) * m2.t + m3
    })
    assert(res == DenseMatrix(List(List(55, 72), List(79, 104))), "\n" + res.toAscii)
  }

  test("various expressions") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    var res = opt(alloc, {
      m * m
    })
    res = opt(alloc, {
      m + m
    })
    res = opt(alloc, {
      (m + m) * m
    })
    res = opt(alloc, {
      m.t
    })
  }

  test("A * B * C") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3), List(3, 3)))
    val m4 = DenseMatrix(List(List(1, 0), List(0, 1)))
    val m5 = DenseMatrix(List(List(0, 0), List(0, 0)))
    val res = opt(alloc, {
      val d = m3 * m4 + m5
      val c = m3 * m4
      m1 * m2 * c * d
    })
    assert(res == DenseMatrix(List(List(144, 144), List(144, 144))))
  }

  test("cse example") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3), List(3, 3)))
    val m4 = DenseMatrix(List(List(1, 0), List(0, 1)))
    val res = opt(alloc, {
      val a = m1 * m2
      val b = a * m3
      val c = b * 4
      c + b
    })
    assert(res == DenseMatrix(List(List(120, 120), List(120, 120))))
  }

  test("opt 1") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3), List(3, 3)))
    val m4 = DenseMatrix(List(List(1, 0), List(0, 1)))
    val res = opt(alloc, {
      val a = m1 + m4
      m2 * m3 + 2 * a
    })
    assert(res == DenseMatrix(List(List(16, 14), List(14, 16))))
  }

  test("opt maybeUpdateToMulRes 1") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3), List(3, 3)))
    val m4 = DenseMatrix(List(List(1, 0), List(0, 1)))
    val res = opt(alloc, {
      val a = m1 * m2
      val b = a * m3
      b * m4
    })
    assert(res == DenseMatrix(List(List(24, 24), List(24, 24))))
  }

  test("opt maybeUpdateToMulRes 2") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3, 3), List(3, 3, 3)))
    val m4 = DenseMatrix(List(List(1, 0, 1), List(0, 1, 2), List(1, 2, 3)))
    val res = opt(alloc, {
      val a = m1 * m2
      val b = a * m3
      b * m4
    })
    assert(res == DenseMatrix(List(List(48, 72, 144), List(48, 72, 144))))
  }

  test("opt inplace scalar mul") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3), List(3, 3)))
    val m4 = DenseMatrix(List(List(1, 0), List(0, 1)))
    val res = opt(alloc, {
      val a = m1 * m2
      val b = a * m3
      b * 4
    })
    assert(res == DenseMatrix(List(List(96, 96), List(96, 96))))
  }

  test("opt maybeUpdateToScalaRes 1") {
    val m1 = DenseMatrix(List(List(1, 1), List(1, 1)))
    val m2 = DenseMatrix(List(List(2, 2), List(2, 2)))
    val m3 = DenseMatrix(List(List(3, 3), List(3, 3)))
    val m4 = DenseMatrix(List(List(1, 0), List(0, 1)))
    val res = opt(alloc, {
      val c = m1
      val d = c * 4
      c + d
    })
    println(res.toAscii)
    assert(res == DenseMatrix(List(List(120, 120), List(120, 120))))
  }

  test("offheapExampleU10") {
    val offheapMatrix1_10 = DenseMatrix.uninit(10, 10)
    val offheapMatrix2_10 = DenseMatrix.uninit(10, 10)
    val offheapMatrix3_10 = DenseMatrix.uninit(10, 10)
    val offheapMatrix4_10 = DenseMatrix.uninit(10, 10)
    opt(malloc, {
      val a = offheapMatrix1_10
      val b = offheapMatrix2_10
      val c = offheapMatrix3_10
      val d = offheapMatrix4_10
      val x = 7.0 * (a + (b + c))
      val y = 4.0 * (c * (b + c))
      x + y
    })
  }

  test("new matrix") {
    val res = opt(alloc, {
      val a = DenseMatrix(List(List(1, 2), List(3, 4)))
      val b = DenseMatrix(List(List(1, 0), List(0, 1)))
      a + b
    })
    assert(res == DenseMatrix(List(List(2, 2), List(3, 5))))
  }
}
