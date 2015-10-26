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
}
