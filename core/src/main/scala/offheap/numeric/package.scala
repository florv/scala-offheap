package scala.offheap

import scala.offheap.internal.macros

package object numeric extends OffheapPackage {
  import scala.language.experimental.{macros => canMacro}

  def opt[S, T](alloc: S, t: T): T = macro macros.NumericMethod.opt

  implicit def Double2DenseMatrixRichDouble(value: Double): DenseMatrixRichDouble =
    new DenseMatrixRichDouble(value)
}
