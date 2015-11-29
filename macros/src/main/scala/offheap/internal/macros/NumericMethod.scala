package scala.offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

class NumericMethod(val c: blackbox.Context) extends Common {
  import c.universe._

  val DenseMatrixClass = rootMirror.staticClass("scala.offheap.numeric.DenseMatrix")
  //val AllocatorClass = rootMirror.staticClass("scala.offheap.Allocator")

  // TODO: add other ops (matrix, scalar, vector)
  sealed abstract class Op
  final case class ConstantLeaf(tree: Tree) extends Op
  final case class MatrixLeaf(tree: Tree) extends Op
  final case class AllocatorLeaf(tree: Tree) extends Op
  final case class MatrixMultiply(left: Op, right: Op, alloc: AllocatorLeaf) extends Op
  final case class MatrixAddition(left: Op, right: Op, alloc: AllocatorLeaf) extends Op
  final case class MatrixTranspose(matrix: Op, alloc: AllocatorLeaf) extends Op
  final case class MatrixScalarMultiply(left: ConstantLeaf, right: Op, alloc: AllocatorLeaf) extends Op
  final case class MatrixCopy(matrix: Op, alloc: AllocatorLeaf) extends Op

  final case class WriteableMatrix(op: Op, alloc: AllocatorLeaf) extends Op

  // C = alpha * A? * B? + beta * C
  final case class DGEMM(alpha: ConstantLeaf, beta: ConstantLeaf, a: Op, at: Op, b: Op, bt: Op, c: Op) extends Op

  // TODO: add other type tests
  def isMatrix(tpe: Type): Boolean = tpe <:< DenseMatrixClass.toType
  def isAllocator(tpe: Type): Boolean = tpe <:< AllocatorClass.toType

  def ensureConstantLeaf(op: Op): ConstantLeaf = op match {
    case cl: ConstantLeaf => cl
    case _ => throw new RuntimeException("ConstantLeaf expected but got " + op.getClass)
  }

  def ensureAllocatorLeaf(op: Op): AllocatorLeaf = op match {
    case al: AllocatorLeaf => al
    case _ => throw new RuntimeException("AllocatorLeaf expected bu get " + op.getClass)
  }

  def toOp(tree: Tree): Op = {
    val q"{ ..$init; $last }" = tree
    val bindings: Map[Symbol, Tree] = init.map {
      case vd @ q"val $_: $_ = $rhs" =>
        (vd.symbol, rhs)
    }.toMap
    // TODO: other conversions
    def loop(expr: Tree): Op = {
      println("loop: " + showCode(expr))
      println("loop: " + showRaw(expr))
      expr match {
        case id: RefTree if bindings.contains(id.symbol) =>
          ???
        case id: RefTree =>
          if (isMatrix(id.tpe)) MatrixLeaf(id)
          else if (isAllocator(id.tpe)) AllocatorLeaf(id)
          else ???
        case q"scala.offheap.numeric.`package`.Double2DenseMatrixRichDouble($v)" => {
          ConstantLeaf(v)
        }
        case q"$a.+($b)($alloc)" => (isMatrix(a.tpe), isMatrix(b.tpe)) match {
          case (true, true) => MatrixAddition(loop(a), loop(b), ensureAllocatorLeaf(loop(alloc)))
          case _ => ???
        }
        case q"$a.*($b)($alloc)" => (isMatrix(a.tpe), isMatrix(b.tpe)) match {
          case (true, true)   => MatrixMultiply(loop(a), loop(b), ensureAllocatorLeaf(loop(alloc)))
          case (false, true)  =>
            MatrixScalarMultiply(ensureConstantLeaf(loop(a)), loop(b), ensureAllocatorLeaf(loop(alloc)))
          case (true, false)  =>
            MatrixScalarMultiply(ensureConstantLeaf(loop(b)), loop(a), ensureAllocatorLeaf(loop(alloc)))
          case (false, false) => ConstantLeaf(expr)
        }
        case q"$a.t()($alloc)" => {
          MatrixTranspose(loop(a), ensureAllocatorLeaf(loop(alloc)))
        }
        case q"$a.copy()($alloc)" => {
          MatrixCopy(loop(a), ensureAllocatorLeaf(loop(alloc)))
        }
        case c: Literal =>
          ConstantLeaf(c)
        case e =>
          throw new Exception("unknown expr: " + showRaw(e))
      }
    }

    loop(last)
  }

  // TODO: preserve original names?
  def toTree(op: Op): Tree = {
    var schedule = List.empty[Tree]
    val scheduleMap = mutable.Map.empty[Op, TermName]
    def loop(op: Op): TermName =
      if (scheduleMap.contains(op))
        scheduleMap(op)
      else
        op match {
          case MatrixLeaf(tree) =>
            val name = fresh("matrix_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case ConstantLeaf(tree) =>
            val name = fresh("constant_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case AllocatorLeaf(tree) =>
            val name = fresh("allocator_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case MatrixMultiply(left, right, alloc) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_multiply")
            schedule = q"val $name = $leftname.*($rightname)(alloc)" :: schedule
            scheduleMap += ((op, name))
            name
          case MatrixAddition(left, right, alloc) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_multiply")
            schedule = q"val $name = $leftname.+($rightname)(alloc)" :: schedule
            scheduleMap += ((op, name))
            name
          case MatrixScalarMultiply(left, right, alloc) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_scalar_multiply")
            schedule = q"val $name = $leftname.*($rightname)(alloc)" :: schedule
            scheduleMap += ((op, name))
            name
          case MatrixTranspose(m, alloc) =>
            val transposed = loop(m)
            val name = fresh("matrix_transpose")
            schedule = q"val $name = $transposed.t()(alloc)" :: schedule
            scheduleMap += ((op, name))
            name
          case MatrixCopy(m, alloc) =>
            val mName = loop(m)
            val allocName = loop(alloc)
            val name = fresh("matrix_copy")
            schedule = q"val $name = $mName.copy()($allocName)" :: schedule
            scheduleMap += ((op, name))
            name
          case WriteableMatrix(m, alloc) =>
            // TODO: If the matrix is not allocated outside the opt block and
            // if all other operations needing it don't need to write and are
            // scheduled earlier we can avoid to copy the matrix
            loop(MatrixCopy(m, alloc))
          case DGEMM(alpha, beta, a, at, b, bt, c) =>
            val alphaName = loop(alpha)
            val betaName = loop(beta)
            val aName = loop(a)
            val atName = loop(at)
            val bName = loop(b)
            val btName = loop(bt)
            val cName = loop(c)
            println("b = " + b + ", c = " + c)
            println("bName = " + bName + ", cName = " + cName)
            schedule = q"$cName.dgemm($alphaName, $betaName, $aName, $atName, $bName, $btName)" :: schedule
            scheduleMap --= scheduleMap.filter(_._2 == cName).keys
            scheduleMap += ((op, cName))
            cName
        }

    val last = loop(op)
    q"{ ..${schedule.reverse}; $last }"
  }

  // TODO: Global Value Numbering
  // TODO: Common Subexpression Elimination
  // TODO: Strength Reduction
  // TODO: Eliminate Intermediate Results
  def optimise(op: Op): Op = {
    op match {
      // TODO: Use unitialized matrix for C

      // (v * A') * B' + w * C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), MatrixTranspose(b, _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // (v * A') * B + w * C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), b, _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // (v * A) * B' + w * C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, a, _), MatrixTranspose(b, _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // (v * A) * B + w * C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, a, _), b, _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // A' * (v * B') + w * C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, MatrixTranspose(b, _), _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // A' * (v * B) + w * C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, b, _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // A * (v * B') + w * C
      case MatrixAddition(MatrixMultiply(a, MatrixScalarMultiply(v, MatrixTranspose(b, _), _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // A * (v * B) + w * C
      case MatrixAddition(MatrixMultiply(a, MatrixScalarMultiply(v, b, _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(v, w, a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))

      // (v * A') * B' + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), MatrixTranspose(b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // (v * A') * B + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), b, _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // (v * A) * B' + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, a, _), MatrixTranspose(b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // (v * A) * B + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, a, _), b, _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // A' * (v * B') + C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, MatrixTranspose(b, _), _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // A' * (v * B) + C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // A * (v * B') + C
      case MatrixAddition(MatrixMultiply(a, MatrixScalarMultiply(v, MatrixTranspose(b, _), _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // A * (v * B) + C
      case MatrixAddition(MatrixMultiply(a, MatrixScalarMultiply(v, b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))

      // (v * A') * B' + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), MatrixTranspose(b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // (v * A') * B + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), b, _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // (v * A) * B' + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, a, _), MatrixTranspose(b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // (v * A) * B + C
      case MatrixAddition(MatrixMultiply(MatrixScalarMultiply(v, a, _), b, _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // A' * (v * B') + C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, MatrixTranspose(b, _), _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // A' * (v * B) + C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(c, alloc))
      // A * (v * B') + C
      case MatrixAddition(MatrixMultiply(a, MatrixScalarMultiply(v, MatrixTranspose(b, _), _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(c, alloc))
      // A * (v * B) + C
      case MatrixAddition(MatrixMultiply(a, MatrixScalarMultiply(v, b, _), _), c, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"1.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))

      // (v * A') * B'
      case MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), MatrixTranspose(b, _), alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(b, alloc))
      // (v * A') * B
      case MatrixMultiply(MatrixScalarMultiply(v, MatrixTranspose(a, _), _), b, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))
      // (v * A) * B'
      case MatrixMultiply(MatrixScalarMultiply(v, a, _), MatrixTranspose(b, _), alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(b, alloc))
      // (v * A) * B
      case MatrixMultiply(MatrixScalarMultiply(v, a, _), b, alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))
      // A' * (v * B')
      case MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, MatrixTranspose(b, _), _), alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"true"), WriteableMatrix(b, alloc))
      // A' * (v * B)
      case MatrixMultiply(MatrixTranspose(a, _), MatrixScalarMultiply(v, b, _), alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"true"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))
      // A * (v * B')
      case MatrixMultiply(a, MatrixScalarMultiply(v, MatrixTranspose(b, _), _), alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"true"), WriteableMatrix(b, alloc))
      // A * (v * B)
      case MatrixMultiply(a, MatrixScalarMultiply(v, b, _), alloc) =>
        DGEMM(v, ensureConstantLeaf(toOp(q"0.0d")), a, ConstantLeaf(q"false"), b, ConstantLeaf(q"false"), WriteableMatrix(b, alloc))

      // A' * B' + C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixTranspose(b, _), _), c, alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), ensureConstantLeaf(toOp(q"1.0d")),
              a, ConstantLeaf(q"true"),
              b, ConstantLeaf(q"true"),
              WriteableMatrix(c, alloc))
      // A' * B + C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), b, _), c, alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), ensureConstantLeaf(toOp(q"1.0d")),
              a, ConstantLeaf(q"true"),
              b, ConstantLeaf(q"false"),
              WriteableMatrix(c, alloc))
      // A * B' + C
      case MatrixAddition(MatrixMultiply(a, MatrixTranspose(b, _), _), c, alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), ensureConstantLeaf(toOp(q"1.0d")),
              a, ConstantLeaf(q"false"),
              b, ConstantLeaf(q"true"),
              WriteableMatrix(c, alloc))
      // A * B + C
      case MatrixAddition(MatrixMultiply(a, b, _), c, alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), ensureConstantLeaf(toOp(q"1.0d")),
              a, ConstantLeaf(q"false"),
              b, ConstantLeaf(q"false"),
              WriteableMatrix(c, alloc))

      // A' * B' + w * C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), MatrixTranspose(b, _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), w,
              a, ConstantLeaf(q"true"),
              b, ConstantLeaf(q"true"),
              WriteableMatrix(c, alloc))
      // A' * B + w * C
      case MatrixAddition(MatrixMultiply(MatrixTranspose(a, _), b, _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), w,
              a, ConstantLeaf(q"true"),
              b, ConstantLeaf(q"false"),
              WriteableMatrix(c, alloc))
      // A * B' + w * C
      case MatrixAddition(MatrixMultiply(a, MatrixTranspose(b, _), _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), w,
              a, ConstantLeaf(q"false"),
              b, ConstantLeaf(q"true"),
              WriteableMatrix(c, alloc))
      // A * B + w * C
      case MatrixAddition(MatrixMultiply(a, b, _), MatrixScalarMultiply(w, c, _), alloc) =>
        DGEMM(ensureConstantLeaf(toOp(q"1.0d")), w,
              a, ConstantLeaf(q"false"),
              b, ConstantLeaf(q"false"),
              WriteableMatrix(c, alloc))

      case _ => op
    }
  }

  def opt(t: Tree): Tree = {
    val op = toOp(t)
    println(s"parsed op: $op")
    val optOp = optimise(op)
    println(s"optimised op: $optOp")
    val res = toTree(optOp)
    println(s"macro result: ${showCode(res)}")
    res
  }
}
