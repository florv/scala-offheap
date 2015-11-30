package scala.offheap

package object numeric extends OffheapPackage {
  implicit def Double2DenseMatrixRichDouble(value: Double): DenseMatrixRichDouble =
    new DenseMatrixRichDouble(value)
}
