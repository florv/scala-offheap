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

  ignore("dot product") {
    //val a = DenseMatrix(List(List(1.0), List(2.0), List(3.0)))
    //val b = DenseMatrix(List(List(4.0), List(5.0), List(6.0)))
    val a = DenseMatrix(List(List(1.0), List(2.0), List(4.0), List(8.0), List(16.0), List(32.0)))
    println(a)
    val b = DenseMatrix(List(List(2.0), List(1.0), List(1.0), List(1.0), List(1.0), List(32.0)))
    val res = a.dot(b)
    assert(res == 32)
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
    assert(a(0, 0) == -2)
    assert(a(0, 1) == 1)
    assert(a(1, 0) == 1.5)
    assert(a(1, 1) == -0.5)
  }
}
