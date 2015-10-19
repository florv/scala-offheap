package scala.offheap.numeric

import scala.offheap._

class DenseMatrix private (val addr: Addr) extends AnyVal {

  private def dataAddr = addr + 3 * strideOf[Int]
  private def stride = Memory.getInt(this.addr + 2 * strideOf[Int])

  private def setMetadata(lines: Int, columns: Int, stride: Int): Unit = {
    Memory.putInt(this.addr, lines)
    Memory.putInt(this.addr + strideOf[Int], columns)
    Memory.putInt(this.addr + 2 * strideOf[Int], stride)
  }

  def *(m: DenseMatrix) {
  }

  def +(m: DenseMatrix) {
  }
}

object DenseMatrix {

  def fromAddr(addr: Addr): DenseMatrix = new DenseMatrix(addr)

  def uninit(lines: Int, columns: Int)(implicit a: Allocator): DenseMatrix = {
    if (lines <= 0) throw new IllegalArgumentException("Number of lines must be strictly positive")
    if (columns <= 0) throw new IllegalArgumentException("Number of colums must be strictly positive")
    val size = 3 * strideOf[Int] + lines * columns * strideOf[Float]
    val addr = a.allocate(size)
    val stride = lines
    val m = fromAddr(addr)
    m.setMetadata(lines, columns, stride)
    return m
  }

  def zeros(lines: Int, columns: Int)(implicit a: Allocator): DenseMatrix = {
    val m = uninit(lines, columns)
    Memory.zero(m.addr, columns * m.stride * strideOf[Int])
    return m
  }
}
