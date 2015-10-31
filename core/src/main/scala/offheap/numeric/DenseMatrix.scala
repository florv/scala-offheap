package scala.offheap.numeric

import scala.offheap._
import scala.offheap.numeric.jni._

class DenseMatrix private (val addr: Addr) extends AnyVal {

  private def dataAddr = addr + 3 * strideOf[Int]
  private def stride = Memory.getInt(this.addr + 2 * strideOf[Int])
  def rows = Memory.getInt(this.addr)
  def columns = Memory.getInt(this.addr + strideOf[Int])

  private def setMetadata(rows: Int, columns: Int, stride: Int): Unit = {
    Memory.putInt(this.addr, rows)
    Memory.putInt(this.addr + strideOf[Int], columns)
    Memory.putInt(this.addr + 2 * strideOf[Int], stride)
  }

  private def addrOf(row: Int, column: Int): Addr = {
    if (row < 0 || row >= rows) {
      throw new IllegalArgumentException(
        "Row index " + row + " is out of bounds [0, " + (rows - 1) + "]")
    } else if (column < 0 || column >= columns) {
      throw new IllegalArgumentException(
        "Column index " + column + " is out of bounds [0, " + (columns - 1) + "]")
    }
    dataAddr + row * strideOf[Double] + stride * strideOf[Double] * column
  }

  def apply(row: Int, column: Int): Double = {
    Memory.getDouble(addrOf(row, column))
  }

  def update(row: Int, column: Int, value: Double): Unit = {
    Memory.putDouble(addrOf(row, column), value)
  }

  def *(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    val res = DenseMatrix.uninit(rows, m.columns);
    val transa: Int = LapackJNI.CblasNoTrans
    val transb: Int = LapackJNI.CblasNoTrans
    val alpha: Double = 1
    val beta: Double = 0
    // TODO Throw exception unless (this.columns == m.rows)
    // C = alpha*op( A )*op( B ) + beta*C
    LapackJNI.cblas_dgemm(LapackJNI.CblasColMajor, transa, transb, this.rows, m.columns, this.columns,
      1.0, this.dataAddr, this.stride, m.dataAddr, m.stride, 0.0, res.dataAddr, res.stride)
    res
  }

  def +(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    if (m.rows != rows || m.columns != columns) {
      throw new IllegalArgumentException("Matrices dimensions dont match: "
        + "left operand is " + rows + "×" + columns
        + "while right operand is " + m.rows + "×" + m.columns)
    }
    val res = DenseMatrix.uninit(rows, columns)
    for (c <- 0 until columns) {
      for (r <- 0 until rows) {
        res.update(r, c, apply(r, c) + m(r, c))
      }
    }
    res
  }
}

object DenseMatrix {

  def fromAddr(addr: Addr): DenseMatrix = new DenseMatrix(addr)

  def apply(es: List[List[Double]])(implicit a: Allocator): DenseMatrix = {
    val rows = es.size
    val columns = if (rows > 0) es(0).size else 0
    for (row <- es) {
      if (row.size != columns) {
        throw new IllegalArgumentException(
          "Can't construct a matrix from variably sized lists")
      }
    }
    val m = DenseMatrix.uninit(rows, columns)
    for ((row, r) <- es.zipWithIndex) {
      for ((e, c) <- row.zipWithIndex) {
        m.update(r, c, e)
      }
    }
    m
  }

  def uninit(rows: Int, columns: Int)(implicit a: Allocator): DenseMatrix = {
    if (rows <= 0) throw new IllegalArgumentException("Number of rows must be strictly positive")
    if (columns <= 0) throw new IllegalArgumentException("Number of columns must be strictly positive")
    val size = 3 * strideOf[Int] + rows * columns * strideOf[Double]
    val addr = a.allocate(size)
    val stride = rows
    val m = fromAddr(addr)
    m.setMetadata(rows, columns, stride)
    return m
  }

  def zeros(rows: Int, columns: Int)(implicit a: Allocator): DenseMatrix = {
    val m = uninit(rows, columns)
    Memory.zero(m.dataAddr, columns * m.stride * strideOf[Double])
    return m
  }
}
