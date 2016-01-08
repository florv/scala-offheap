package test

import org.scalatest.FunSuite
import scala.offheap._
import scala.offheap.numeric._

class DenseMatrixSuite extends FunSuite {
  implicit val alloc = malloc

  test("matrix of zeros") {
    val m = DenseMatrix.zeros(2, 3)
    assert(m.rows == 2)
    assert(m.columns == 3)
    for (c <- 0 to 1) {
      for (l <- 0 to 2) {
        assert(m(c, l) == 0)
      }
    }
  }

  test("matrix update") {
    val m = DenseMatrix.zeros(2, 2)
    m.update(0, 0, 1)
    assert(m(0, 0) == 1)
    m.update(0, 1, 2)
    assert(m(0, 1) == 2)
    m.update(1, 0, 3)
    assert(m(1, 0) == 3)
    m.update(1, 1, 4)
    assert(m(1, 1) == 4)
  }

  test("inline matrix creation") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    assert(m(0, 0) == 1)
    assert(m(0, 1) == 2)
    assert(m(1, 0) == 3)
    assert(m(1, 1) == 4)
  }

  test("matrix addition") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    val res = m + m
    assert(res(0, 0) == 2)
    assert(res(0, 1) == 4)
    assert(res(1, 0) == 6)
    assert(res(1, 1) == 8)
  }

  test("matrix multiplication") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    val res = m * m
    assert(res(0, 0) == 7)
    assert(res(0, 1) == 10)
    assert(res(1, 0) == 15)
    assert(res(1, 1) == 22)
  }

  test("matrix multiplication dimensions mismatch") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    val n = DenseMatrix(List(List(1, 2), List(3, 4), List(5, 6)))
    intercept[IllegalArgumentException] {
      m * n
    }
  }

  /*
  test("breeze") {
    import breeze.linalg.DenseMatrix
    import breeze.linalg._
    val m: breeze.linalg.DenseMatrix[Double] = DenseMatrix((0.0, 2.0), (0.0, 0.0), (1.0, 0.0))
    val res = inv(m)
  }
  */

  test("dot product") {
    val a = DenseMatrix(List(List(1.0), List(2.0), List(4.0), List(8.0), List(16.0), List(32.0)))
    val b = DenseMatrix(List(List(2.0), List(1.0), List(1.0), List(1.0), List(1.0), List(32.0)))
    val res = a.dot(b)
    assert(res == 1056.0)
  }

  test("matrix equality") {
    val a = DenseMatrix(List(List(1, 2), List(3, 4)))
    val b = DenseMatrix(List(List(1, 2), List(3, 4)))
    assert(a == b)
  }

  test("matrix equality2") {
    val a = DenseMatrix(List(List(1, 2), List(3, 4)))
    val b = DenseMatrix(List(List(1, 2, 3), List(3, 4, 5)))
    assert(a != b, b != a)
  }

  test("matrix slice") {
    val a = DenseMatrix(List(List(1, 2, 3, 4), List(5, 6, 7, 8), List(9, 10, 11, 12), List(13, 14, 15, 16)))
    val b = a(0 to 1, 0 to 1)
    assert(b == DenseMatrix(List(List(1, 2), List(5, 6))))
  }

  test("solve") {
    val a = DenseMatrix(List(List(1, 2), List(2, 3)))
    val b = DenseMatrix(List(List(5, 6), List(8, 10)))
    val x = a \ b
    assert(x == DenseMatrix(List(List(1, 2), List(2, 2))))
  }

  test("determinant") {
    val a = DenseMatrix(List(List(1, 2), List(3, 4)))
    assert(a.det == -2)
  }

  test("inverse") {
    val a = DenseMatrix(List(List(1, 2), List(3, 4)))
    val expect = DenseMatrix(List(List(-2, 1), List(1.5, -0.5)))
    val res = a.inv - expect
    assert(res.norm < 1e-14, "Got \n" + res.toAscii + " while exected is\n"
      + expect.toAscii)
  }

  test("inverse of singular matrix") {
    val a = DenseMatrix(List(List(1, 1), List(1, 1)))
    intercept[MatrixSingularException] {
      a.inv()
    }
  }

  test("inverse of non square matrix") {
    val a = DenseMatrix(List(List(1, 2), List(3, 4), List(5, 6)))
    intercept[IllegalArgumentException] {
      a.inv()
    }
  }

  test("big multiply bug around 256") {
    val phi2 = DenseMatrix.ones(400, 5)
    val w2 = DenseMatrix.ones(5, 24)

    val theta2 = (phi2 * w2)//.toDenseMatrix
    assert(theta2(256,0) != 0)
  }

  test("matrix multiplication by scalar on the left") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    val res = 2 * m
    assert(res == DenseMatrix(List(List(2, 4), List(6, 8))))
  }

  test("matrix multiplication by scalar on the right") {
    val m = DenseMatrix(List(List(1, 2), List(3, 4)))
    val res = m * 2
    assert(res == DenseMatrix(List(List(2, 4), List(6, 8))))
  }

  test("Newton-Raphson") {
    def f(v: DenseMatrix): DenseMatrix = {
      val a = math.pow(v(0, 0), 2) - 2 * v(0, 0) + math.pow(v(1, 0), 2) - v(2, 0) + 1
      val b = v(0, 0) * math.pow(v(1, 0), 2) - v(0, 0) - 3 * v(1, 0) + v(1, 0) * v(2, 0) + 2
      val c = v(0, 0) * math.pow(v(2, 0), 2) - 3 * v(2, 0) + v(1, 0) * math.pow(v(2, 0), 2) + v(0, 0) * v(1, 0)
      DenseMatrix(List(List(a), List(b), List(c)))
    }

    def j(v: DenseMatrix): DenseMatrix = {
      val a = 2 * v(0, 0) - 2
      val b = 2 * v(1, 0)
      val c = -1
      val d = math.pow(v(1, 0), 2) - 1
      val e = 2 * v(0, 0) * v(1, 0) - 3 + v(2, 0)
      val f = v(1, 0)
      val g = math.pow(v(2, 0), 2) + v(1, 0)
      val h = math.pow(v(2, 0), 2) + v(0, 0)
      val i = 2 * v(0, 0) * v(2, 0) - 3 + 2 * v(1, 0) * v(2, 0)
      DenseMatrix(List(List(a, b, c), List(d, e, f), List(g, h, i)))
    }

    def newt(x: DenseMatrix, f: DenseMatrix => DenseMatrix, j: DenseMatrix => DenseMatrix): DenseMatrix = {
      val fn = f(x)
      val jn = j(x)
      val d = jn.inv * fn
      val xn = x - d
      if (d.norm < 1e-14) xn else newt(xn, f, j)
    }

    val x0 = DenseMatrix(List(List(1), List(2), List(3)))
    val res = newt(x0, f, j)
    assert(f(res).norm < 1e-14)

    val x1 = DenseMatrix(List(List(-1), List(-1), List(-1)))
    val res1 = newt(x1, f, j)
    assert(f(res1).norm < 1e-14)

    assert(res1 != res)
  }
}
