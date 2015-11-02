package scala.offheap.numeric

import scala.offheap._
import scala.offheap.numeric.jni._

// TODO:
//   - Don't assume stride size match (for instance when copying)
//   - Methods to extract submatrices

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

  private def inplaceOpNoAlloc(op: DenseMatrix, f: (Double, Double) => Double, res: DenseMatrix): Unit = {
    if (op.rows != rows || op.columns != columns) {
      throw new IllegalArgumentException("Matrices dimensions don't match: "
        + "left operand is " + rows + "×" + columns
        + "while right operand is " + op.rows + "×" + op.columns)
    }
    for (col <- 0 until columns) {
      for (row <- 0 until rows) {
        res(row, col) = f(apply(row, col), op(row, col))
      }
    }
  }

  private def inplaceOp(op: DenseMatrix, f: (Double, Double) => Double)(implicit a: Allocator): DenseMatrix = {
    val res = DenseMatrix.uninit(rows, columns)
      inplaceOpNoAlloc(op, f, res)
    res
  }

  def apply(row: Int, column: Int): Double = {
    Memory.getDouble(addrOf(row, column))
  }

  def update(row: Int, column: Int, value: Double): Unit = {
    Memory.putDouble(addrOf(row, column), value)
  }

  def updateAll(f: (Int, Int) => Double): Unit = {
    for (col <- 0 until columns) {
      for (row <- 0 until columns) {
        update(row, col, f(row, col))
      }
    }
  }

  /** Return true if all elements of the matrices are pairwise equal */
  def ==(m: DenseMatrix): Boolean = {
    for (col <- 0 until columns) {
      for (row <- 0 until rows) {
        if (apply(row, col) != m(row, col)) {
          return false
        }
      }
    }
    return true
  }

  /** Matrix multiplication */
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

  /** Dot product
   *
   * This method automatically transpose the argument if necessary. */
  def dot(m: DenseMatrix)(implicit a: Allocator): Double = {
    if (columns != 1 && rows != 1) {
      throw new IllegalArgumentException("This matrix is not a vector")
    }
    if (m.columns != 1 && rows != 1) {
      throw new IllegalArgumentException("The argument is not a vector")
    }
    val n = if (columns != 1) columns else rows
    if (m.columns != n && m.rows != n) {
      throw new IllegalArgumentException("The vector sizes don't match")
    }
    val incx = if (columns == 1) 1 else stride
    val incy = if (m.columns == 1) 1 else m.stride
    LapackJNI.cblas_ddot(n, dataAddr, incx, m.dataAddr, incy)
  }

  /** Element-wise addition */
  def +(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => a + b)
  }

  /** In-place element-wise addition */
  def +=(m: DenseMatrix): Unit = {
    inplaceOpNoAlloc(m, (a, b) => a + b, this)
  }

  /** Element-wise multiplication */
  def :*(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => a * b)
  }

  /** Element-wise division */
  def :/(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => a / b)
  }

  /** Element-wise &lt; comparison */
  def :<(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a < b) 1.0 else 0.0)
  }

  /** Element-wise &lt;= comparison */
  def :<=(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a <= b) 1.0 else 0.0)
  }

  /** Element-wise &gt; comparison */
  def :>(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a > b) 1.0 else 0.0)
  }

  /** Element-wise &gt;= comparison */
  def :>=(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a >= b) 1.0 else 0.0)
  }

  /** Element-wise == comparison */
  def :==(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a == b) 1.0 else 0.0)
  }

  /** In-place addition (mutates this) */
  def :+=(v: Double): Unit = {
    inplace(x => x + v)
  }

  /** In-place multiplication (mutates this) */
  def :*=(v: Double): Unit = {
    inplace(x => x * v)
  }

  /** Element-wise &amp; */
  def :&(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a != 0.0 && b != 0.0) 1.0 else 0.0)
  }

  /** Element-wise | */
  def :|(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    inplaceOp(m, (a, b) => if (a != 0.0 || b != 0.0) 1.0 else 0.0)
  }

  /** Element-wise not */
  def !()(implicit a: Allocator): DenseMatrix = {
    val res = copy
    res.inplace(a => if (a == 0.0) 1.0 else 0.0)
    res
  }

  /** Return true if any element is non-zero */
  def any(): Boolean = {
    for (col <- 0 until columns) {
      for (row <- 0 until rows) {
        if (apply(row, col) != 0.0) {
          return true
        }
      }
    }
    return false
  }

  /** Return true if all elements are non-zero */
  def all(): Boolean = {
    for (col <- 0 until columns) {
      for (row <- 0 until rows) {
        if (apply(row, col) == 0.0) {
          return false
        }
      }
    }
    return true
  }

  /** Apply the given function on each element, in-place. */
  def inplace(f: Double => Double): Unit = {
    for (col <- 0 until columns) {
      for (row <- 0 until columns) {
        update(row, col, f(apply(row, col)))
      }
    }
  }

  def round()(implicit a: Allocator): DenseMatrix = {
    val res = copy
    res.inplace(math.round(_).toDouble)
    res
  }

  def ceil()(implicit a: Allocator): DenseMatrix = {
    val res = copy
    res.inplace(math.ceil)
    res
  }

  def floor()(implicit a: Allocator): DenseMatrix = {
    val res = copy
    res.inplace(math.floor)
    res
  }

  def signum()(implicit a: Allocator): DenseMatrix = {
    val res = copy
    inplace(math.signum)
    res
  }

  def abs()(implicit a: Allocator): DenseMatrix = {
    val res = copy
    inplace(math.abs)
    res
  }

  /** Find max */
  def max(): Double = {
    ???
  }

  /** Find min */
  def min(): Double = {
    ???
  }

  /** Coordinates of the maximum */
  def argmax(): (Int, Int) = {
    ???
  }

  /** Coordinates of the minimum */
  def argmin(): (Int, Int) = {
    ???
  }

  /** Sum of the elemens */
  def sum(): Double = {
    var s: Double = 0
    for (col <- 0 until columns) {
      for (row <- 0 until rows) {
        s += apply(row, col)
      }
    }
    s
  }

  /** Trace of the matrix */
  def trace(): Double = {
    var t: Double = 0
    for (i <- 0 until math.max(rows, columns)) {
      t += apply(i, i)
    }
    t
  }

  /** Cumulative sum */
  def accumulate(): Double = {
    ???
  }

  /** Return a transposed copy of the matrix. */
  def t(): DenseMatrix = {
    ???
  }

  /** Return a copy of the matrix. */
  def copy()(implicit a: Allocator): DenseMatrix = {
    val m = DenseMatrix.uninit(rows, columns)
    Memory.copy(dataAddr, m.dataAddr, columns * m.stride * strideOf[Double])
    m
  }

  /** Copy the lower triangular portion in a new matrix. */
  def lowerTriangular()(implicit a: Allocator): DenseMatrix = {
    ???
  }

  /** Copy the upper triangular portion in a new matrix. */
  def upperTriangular()(implicit a: Allocator): DenseMatrix = {
    ???
  }

  /** Concatenate a matrix underneath. */
  def vertcat(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    ???
  }

  /** Concatenate a matrix on the right. */
  def horzcat(m: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    if (m.rows != rows) {
      throw new IllegalArgumentException("Number of rows do not match: " + rows +
        "×" + columns + "on the left while " + m.rows + "×" + m.columns + "on the right")
    }
    val res = DenseMatrix.uninit(rows, columns + m.columns)
    if (m.stride == stride && res.stride == stride) {
      val len1 = columns * stride * strideOf[Double]
      val len2 = m.columns * m.stride * strideOf[Double]
      Memory.copy(dataAddr, res.dataAddr, len1)
      Memory.copy(m.dataAddr, res.dataAddr + len1, len2)
    } else {
      /* Copy each column individually */
      ???
    }
    res
  }

  /** Solve linear system
   *
   * Solve this * x = b and return x */
  def \(b: DenseMatrix)(implicit a: Allocator): DenseMatrix = {
    ???
  }

  /** Determinant */
  def det(): Double = {
    ???
  }

  /** Matrix inverse */
  def inv()(implicit a: Allocator): DenseMatrix = {
    ???
  }

  /** Moore-Penrose pseudoinverse */
  def pinv()(implicit a: Allocator): DenseMatrix = {
    ???
  }

  /** Vector frobenius norm */
  def norm(): Double = {
    ???
  }

  /* TODO: Eigenvectors and eigenvalues */
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
    m
  }

  /** Create a matrix of zeroes. */
  def zeros(rows: Int, columns: Int)(implicit a: Allocator): DenseMatrix = {
    val m = uninit(rows, columns)
    Memory.zero(m.dataAddr, columns * m.stride * strideOf[Double])
    m
  }

  /** Create a vector of zeroes. */
  def zeros(rows: Int)(implicit a: Allocator): DenseMatrix = {
    zeros(rows, 1)
  }

  /** Create a vector of ones. */
  def ones(rows: Int)(implicit a: Allocator): DenseMatrix = {
    val m = uninit(rows, 1)
    m.inplace(_ => 1)
    m
  }

  /** Create a square matrix with the given elements on the diagonal. */
  def diag(d: List[Double])(implicit a: Allocator): DenseMatrix = {
    var m = uninit(d.size, d.size)
    for (i <- 0 until d.size) {
      m(i, i) = d(i)
    }
    m
  }

  /** Create a new matrix, filling it with the result of the given function. */
  def tabulate(rows: Int, columns: Int)(f: (Int, Int) => Double)(implicit a: Allocator): DenseMatrix = {
    var m = uninit(rows, columns)
    m.updateAll(f)
    m
  }

  /** Create a new vector, filling it with the result of the given function. */
  def tabulate(rows: Int)(f: Int => Double)(implicit a: Allocator): DenseMatrix = {
    tabulate(rows, 1)((r, c) => f(r))
  }

  /** Create a matrix of random elements in the range [0, 1]. */
  def rand(rows: Int, columns: Int)(implicit a: Allocator): DenseMatrix = {
    var m = uninit(rows, columns)
    m.updateAll((_, _) => math.random)
    m
  }

  /** Create a vector of random elements in the range [0, 1]. */
  def rand(rows: Int)(implicit a: Allocator): DenseMatrix = {
    rand(rows, 1)
  }

}
