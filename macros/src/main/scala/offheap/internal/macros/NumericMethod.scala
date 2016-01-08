package scala.offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.blackbox

/* This class essentially defines the opt(allocator, block) macro.
 * That will optimised the operations with DenseMatrices present in block.
 *
 * The optimisation is done in several stages:
 *   1. Common subexpression elimination
 *   2. Strength reduction
 *   3. Conversion to static single assignment form
 *   4. Scheduling (producing a scala tree)
 */
class NumericMethod(val c: blackbox.Context) extends Common {
  import c.universe._

  /* There are multiple Op classes, one for each stage of the optimisation.
   * While this slightly increase the verbosity of the code it has the huge
   * benefit that every match statement bellow as a meaningful way to handle
   * every case's. Thus "case _ =>" is avoided entirely and the compiler will
   * always produce usefull warnings.
   *
   * Thus if someone wants to add support for another operation, he simply add
   * the relevent subclasses of Op, compile and then add the code to support
   * the new operation wherever the compiler emmits warning.
   * */

  object Unoptimized {
    /* Operations recognized in the block passed to the opt() macro. */
    sealed abstract class Op
    final case class Const(tree: Tree) extends Op
    final case class Mat(tree: Tree) extends Op
    final case class Mat_*(left: Op, right: Op) extends Op
    final case class Mat_+(left: Op, right: Op) extends Op
    final case class Mat_T(matrix: Op) extends Op
    final case class Scalar_*(left: Const, right: Op) extends Op
    final case class NewMat(tree: Tree) extends Op
  }

  object CSE {
    /* After commons subexpression elimination those operation are obtained. */
    sealed abstract class Op
    final case class Ref(name: TermName) extends Op
    final case class Const(tree: Tree) extends Op
    final case class Mat(tree: Tree) extends Op
    final case class Mat_*(left: Op, right: Op) extends Op
    final case class Mat_+(left: Op, right: Op) extends Op
    final case class Mat_T(matrix: Op) extends Op
    final case class Scalar_*(left: Const, right: Op) extends Op
    final case class NewMat(tree: Tree) extends Op
    case class Result(root: Op, named: Map[TermName, Op])
  }

  object SR {
    /* After strength reduction those operations are optained. */
    sealed abstract class Op
    final case class Ref(name: TermName) extends Op
    final case class Const(tree: Tree) extends Op
    final case class Mat(tree: Tree) extends Op
    final case class Mat_*(left: Op, right: Op) extends Op
    final case class Mat_+(left: Op, right: Op) extends Op
    final case class Mat_T(matrix: Op) extends Op
    final case class Scalar_*(left: Const, right: Op) extends Op
    // C = alpha * A? * B? + beta * C
    final case class DGEMM(alpha: Const, beta: Const, a: Op, at: Const, b: Op, bt: Const, c: Op) extends Op
    final case class DGEMM0(alpha: Const, a: Op, at: Const, b: Op, bt: Const) extends Op
    final case class NewMat(tree: Tree) extends Op
    case class Result(root: Op, named: Map[TermName, Op])

    implicit def refFromCSE(r: CSE.Ref): SR.Ref = {
      r match {
        case CSE.Ref(name) => Ref(name)
      }
    }

    implicit def constFromCSE(c: CSE.Const): SR.Const = {
      c match {
        case CSE.Const(tree) => Const(tree)
      }
    }
  }

  object SSA {
    /* The operations in SSA form. */
    sealed abstract class Op
    final case class Assign(name: TermName, op: Op) extends Op
    final case class Const(tree: Tree) extends Op
    final case class Mat(tree: Tree) extends Op
    final case class Mat_*(left: TermName, right: TermName) extends Op
    final case class Mat_+(left: TermName, right: TermName) extends Op
    final case class Mat_T(matrix: TermName) extends Op
    final case class Scalar_*(left: TermName, right: TermName) extends Op
    final case class DGEMM(alpha: Const, beta: Const, a: TermName, at: Const, b: TermName, bt: Const, c: TermName) extends Op
    final case class DGEMM0(alpha: Const, a: TermName, at: Const, b: TermName, bt: Const) extends Op
    final case class NewMat(tree: Tree) extends Op
    final case class Result(ops: List[Op])
  }

  val DenseMatrixClass = rootMirror.staticClass("scala.offheap.numeric.DenseMatrix")

  def isMatrix(tpe: Type): Boolean = tpe <:< DenseMatrixClass.toType
  def isAllocator(tpe: Type): Boolean = tpe <:< AllocatorClass.toType

  def ensureConst(op: Unoptimized.Op): Unoptimized.Const = op match {
    case cl: Unoptimized.Const => cl
    case _ => throw new RuntimeException("Const expected but got " + op.getClass)
  }

  def toOp(tree: Tree): Unoptimized.Op = {
    import Unoptimized._
    val q"{ ..$init; $last }" = tree
    val bindings: Map[Symbol, Tree] = init.map {
      //case vd @ q"val $_: $_ = $rhs" =>
      case vd @ ValDef(_, _, _, rhs) =>
        (vd.symbol, rhs)
    }.toMap
    def loop(expr: Tree): Op = {
      println("loop: " + showCode(expr))
      println("loop: " + showRaw(expr))
      // FIXME: Quasiquotes in pattern appears to be unrelliable: sometimes they don't
      // match as expected. Since I'm unable to reproduce the problem consistently I
      // replaced them by the equivalent scala tree.
      expr match {
        case id: RefTree if bindings.contains(id.symbol) =>
          loop(bindings(id.symbol))
        case id: RefTree =>
          if (isMatrix(id.tpe)) Mat(id)
          else throw new Exception("Reference to type unsuported by opt() macro")
        //case q"scala.offheap.numeric.`package`.Double2DenseMatrixRichDouble($v)" => {
        case Apply(Select(Select(Select(Select(Ident(TermName("scala")), TermName("offheap")), TermName("numeric")), termNames.PACKAGE), TermName("Double2DenseMatrixRichDouble")), List(v)) => {
          Const(v)
        }
        //case q"$a.+($b)(alloc)" => {
        case Apply(Apply(Select(a, TermName("$plus")), List(b)), List(alloc)) => {
        (isMatrix(a.tpe), isMatrix(b.tpe)) match {
            case (true, true) => Mat_+(loop(a), loop(b))
            case _ => throw new Exception("Unsupported types for + in opt() block")
          }
        }
        //case q"$a.*($b)(alloc)" => {
        case Apply(Apply(Select(a, TermName("$times")), List(b)), List(alloc)) => {
          (isMatrix(a.tpe), isMatrix(b.tpe)) match {
            case (true, true)   => Mat_*(loop(a), loop(b))
            case (false, true)  =>
              Scalar_*(ensureConst(loop(a)), loop(b))
            case (true, false)  =>
              Scalar_*(ensureConst(loop(b)), loop(a))
            case (false, false) => Const(expr)
          }
        }
        //case q"$a.t()(alloc)" => {
        case Apply(Apply(Select(a, TermName("t")), List()), List(alloc)) => {
          Mat_T(loop(a))
        }
        // case q"scala.offheap.numeric.DenseMatrix.apply(tree)(alloc)"
        case Apply(Apply(Select(Select(Select(Select(Ident(TermName("scala")), TermName("offheap")), TermName("numeric")), TermName("DenseMatrix")), TermName("apply")), List(tree)), List(alloc)) =>
          // FIXME: It should be an error to use a val defined in the opt block
          // in tree.
          NewMat(tree)
        case c: Literal =>
          Const(c)
        case e =>
          throw new Exception("unknown expr: " + showRaw(e))
      }
    }

    loop(last)
  }
  // " // <- fixes syntax hilighting issues with quasiquotes

  /* Performs common subexpression elimination */
  def cse(op: Unoptimized.Op): CSE.Result = {
    import CSE._

    def toCSEops(op: Unoptimized.Op): Op = {
      op match {
        case Unoptimized.Const(tree) => Const(tree)
        case Unoptimized.Mat(tree) => Mat(tree)
        case Unoptimized.NewMat(tree) => NewMat(tree)
        case Unoptimized.Mat_*(left, right) => Mat_*(toCSEops(left), toCSEops(right))
        case Unoptimized.Mat_+(left, right) => Mat_+(toCSEops(left), toCSEops(right))
        case Unoptimized.Mat_T(matrix) => Mat_T(toCSEops(matrix))
        case Unoptimized.Scalar_*(Unoptimized.Const(left), right) =>
          Scalar_*(Const(left), toCSEops(right))
      }
    }

    /** Return the largest common subexpression */
    def find(op: Op): Option[Op] = {
      val seenOp: mutable.Set[Op] = mutable.Set.empty[Op]
      def markSeen(op: Op): Unit = {
        op match {
          case _: Ref => {}
          case _: Mat => {}
          case _: Const => {}
          case _ => seenOp += op
        }
      }
      def loop(op: Op): Option[Op] = {
        if (seenOp.contains(op)) Some(op)
        else {
          markSeen(op)
          op match {
            case o: Ref => None
            case c: Const => None
            case m: Mat => None
            case m: NewMat => None
            case Mat_*(a, b) => loop(a).orElse(loop(b))
            case Mat_+(a, b) => loop(a).orElse(loop(b))
            case Mat_T(a) => loop(a)
            case Scalar_*(a, b) => loop(a).orElse(loop(b))
          }
        }
      }
      loop(op)
    }

    /** Replace all occurrences of op in tree by a reference to name.
     *  @return the modifield tree. */
    def replace(tree: Op, op: Op, name: TermName): Op = {
      def loop(tree: Op): Op = {
        if (tree == op) Ref(name)
        else tree match {
          case r: Ref => r
          case c: Const => c
          case m: Mat => m
          case m: NewMat => m
          case Mat_*(left, right) => Mat_*(loop(left), loop(right))
          case Mat_+(left, right) => Mat_+(loop(left), loop(right))
          case Mat_T(matrix) => Mat_T(loop(matrix))
          case Scalar_*(left, right) => Scalar_*(left, loop(right))
        }
      }
      loop(tree)
    }

    def loop(primary: Op, others: Map[TermName, Op]): Result = {
      val cs = find(primary)
      cs match {
        case None => Result(primary, others)
        case Some(cs) =>
          println("cs found: " + cs)
          val name = fresh("cse")
          val newPrimary = replace(primary, cs, name)
          val newOthers = others.mapValues(v => replace(v, cs, name))
          loop(newPrimary, newOthers + ((name, cs)))
      }
    }

    loop(toCSEops(op), Map.empty[TermName, Op])
  }

  /** Performs strength reduction */
  def sr(cseResult: CSE.Result): SR.Result = {
    import CSE._
    def sr0(op: Op): SR.Op = {
      op match {
        // (v * A') * B' + w * C
        case Mat_+(Mat_*(Scalar_*(v, Mat_T(a)), Mat_T(b)), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Scalar_*(v, Mat_T(a)), Mat_T(b))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"true"), sr0(c))
        // (v * A') * B + w * C
        case Mat_+(Mat_*(Scalar_*(v, Mat_T(a)), b), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Scalar_*(v, Mat_T(a)), b)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"false"), sr0(c))
        // (v * A) * B' + w * C
        case Mat_+(Mat_*(Scalar_*(v, a), Mat_T(b)), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Scalar_*(v, a), Mat_T(b))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"true"), sr0(c))
        // (v * A) * B + w * C
        case Mat_+(Mat_*(Scalar_*(v, a), b), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Scalar_*(v, a), b)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"false"), sr0(c))
        // A' * (v * B') + w * C
        case Mat_+(Mat_*(Mat_T(a), Scalar_*(v, Mat_T(b))), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Mat_T(a), Scalar_*(v, Mat_T(b)))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"true"), sr0(c))
        // A' * (v * B) + w * C
        case Mat_+(Mat_*(Mat_T(a), Scalar_*(v, b)), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Mat_T(a), Scalar_*(v, b))) =>
          SR.DGEMM(v, w, sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        // A * (v * B') + w * C
        case Mat_+(Mat_*(a, Scalar_*(v, Mat_T(b))), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(a, Scalar_*(v, Mat_T(b)))) =>
          SR.DGEMM(v, w, sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        // A * (v * B) + w * C
        case Mat_+(Mat_*(a, Scalar_*(v, b)), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(a, Scalar_*(v, b))) =>
          SR.DGEMM(v, w, sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        // v * (A' * B') + w * C
        case Mat_+(Scalar_*(v, Mat_*(Mat_T(a), Mat_T(b))), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Scalar_*(v, Mat_*(Mat_T(a), Mat_T(b)))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"true"), sr0(c))
        // v * (A' * B) + w * C
        case Mat_+(Scalar_*(v, Mat_*(Mat_T(a), b)), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Scalar_*(v, Mat_*(Mat_T(a), b))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"true"), sr0(b), SR.Const(q"false"), sr0(c))
        // v * (A * B') + w * C)
        case Mat_+(Scalar_*(v, Mat_*(a, Mat_T(b))), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Scalar_*(v, Mat_*(a, Mat_T(b)))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"true"), sr0(c))
        // v * (A * B) + w * C
        case Mat_+(Scalar_*(v, Mat_*(a, b)), Scalar_*(w, c)) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Scalar_*(v, Mat_*(a, b))) =>
          SR.DGEMM(v, w, sr0(a), SR.Const(q"false"), sr0(b), SR.Const(q"false"), sr0(c))

        // (v * A') * B' + C
        case Mat_+(Mat_*(Scalar_*(v, Mat_T(a)), Mat_T(b)), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(c, Mat_*(Scalar_*(v, Mat_T(a)), Mat_T(b))) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        // (v * A') * B + C
        case Mat_+(Mat_*(Scalar_*(v, Mat_T(a)), b), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(c, Mat_*(Scalar_*(v, Mat_T(a)), b)) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        // (v * A) * B' + C
        case Mat_+(Mat_*(Scalar_*(v, a), Mat_T(b)), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(c, Mat_*(Scalar_*(v, a), Mat_T(b))) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        // (v * A) * B + C
        case Mat_+(Mat_*(Scalar_*(v, a), b), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(c, Mat_*(Scalar_*(v, a), b)) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        // A' * (v * B') + C
        case Mat_+(Mat_*(Mat_T(a), Scalar_*(v, Mat_T(b))), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(c, Mat_*(Mat_T(a), Scalar_*(v, Mat_T(b)))) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        // A' * (v * B) + C
        case Mat_+(Mat_*(Mat_T(a), Scalar_*(v, b)), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(c, Mat_*(Mat_T(a), Scalar_*(v, b))) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        // A * (v * B') + C
        case Mat_+(Mat_*(a, Scalar_*(v, Mat_T(b))), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(c, Mat_*(a, Scalar_*(v, Mat_T(b)))) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        // A * (v * B) + C
        case Mat_+(Mat_*(a, Scalar_*(v, b)), c) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(c, Mat_*(a, Scalar_*(v, b))) =>
          SR.DGEMM(v, Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))

        // (v * A') * B'
        case Mat_*(Scalar_*(v, Mat_T(a)), Mat_T(b)) =>
          SR.DGEMM0(v, sr0(a), Const(q"true"), sr0(b), Const(q"true"))
        // (v * A') * B
        case Mat_*(Scalar_*(v, Mat_T(a)), b) =>
          SR.DGEMM0(v, sr0(a), Const(q"true"), sr0(b), Const(q"false"))
        // (v * A) * B'
        case Mat_*(Scalar_*(v, a), Mat_T(b)) =>
          SR.DGEMM0(v, sr0(a), Const(q"false"), sr0(b), Const(q"true"))
        // (v * A) * B
        case Mat_*(Scalar_*(v, a), b) =>
          SR.DGEMM0(v, sr0(a), Const(q"false"), sr0(b), Const(q"false"))
        // A' * (v * B')
        case Mat_*(Mat_T(a), Scalar_*(v, Mat_T(b))) =>
          SR.DGEMM0(v, sr0(a), Const(q"true"), sr0(b), Const(q"true"))
        // A' * (v * B)
        case Mat_*(Mat_T(a), Scalar_*(v, b)) =>
          SR.DGEMM0(v, sr0(a), Const(q"true"), sr0(b), Const(q"false"))
        // A * (v * B')
        case Mat_*(a, Scalar_*(v, Mat_T(b))) =>
          SR.DGEMM0(v, sr0(a), Const(q"false"), sr0(b), Const(q"true"))
        // A * (v * B)
        case Mat_*(a, Scalar_*(v, b)) =>
          SR.DGEMM0(v, sr0(a), Const(q"false"), sr0(b), Const(q"false"))

        // A' * B' + w * C
        case Mat_+(Mat_*(Mat_T(a), Mat_T(b)), Scalar_*(w, c)) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Mat_T(a), Mat_T(b))) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        // A' * B + w * C
        case Mat_+(Mat_*(Mat_T(a), b), Scalar_*(w, c)) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(Mat_T(a), b)) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        // A * B' + w * C
        case Mat_+(Mat_*(a, Mat_T(b)), Scalar_*(w, c)) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(a, Mat_T(b))) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        // A * B + w * C
        case Mat_+(Mat_*(a, b), Scalar_*(w, c)) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(Scalar_*(w, c), Mat_*(a, b)) =>
          SR.DGEMM(Const(q"1.0d"), w, sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))

        // A' * B' + C
        case Mat_+(Mat_*(Mat_T(a), Mat_T(b)), c) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(c, Mat_*(Mat_T(a), Mat_T(b))) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"), sr0(c))
        // A' * B + C
        case Mat_+(Mat_*(Mat_T(a), b), c) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(c, Mat_*(Mat_T(a), b)) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"), sr0(c))
        // A * B' + C
        case Mat_+(Mat_*(a, Mat_T(b)), c) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        case Mat_+(c, Mat_*(a, Mat_T(b))) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"), sr0(c))
        // A * B + C
        case Mat_+(Mat_*(a, b), c) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))
        case Mat_+(c, Mat_*(a, b)) =>
          SR.DGEMM(Const(q"1.0d"), Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"false"), sr0(c))

        // A' * B'
        case Mat_*(Mat_T(a), Mat_T(b)) =>
          SR.DGEMM0(Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"true"))
        // A' * B
        case Mat_*(Mat_T(a), b) =>
          SR.DGEMM0(Const(q"1.0d"), sr0(a), Const(q"true"), sr0(b), Const(q"false"))
        // A * B'
        case Mat_*(a, Mat_T(b)) =>
          SR.DGEMM0(Const(q"1.0d"), sr0(a), Const(q"false"), sr0(b), Const(q"true"))

        case Ref(name) => SR.Ref(name)
        case Const(tree) => SR.Const(tree)
        case Mat(tree) => SR.Mat(tree)
        case NewMat(tree) => SR.NewMat(tree)
        case Mat_*(left, right) => SR.Mat_*(sr0(left), sr0(right))
        case Mat_+(left, right) => SR.Mat_+(sr0(left), sr0(right))
        case Mat_T(matrix) => SR.Mat_T(sr0(matrix))
        case Scalar_*(left, right) => SR.Scalar_*(left, sr0(right))
      }
    }
    SR.Result(sr0(cseResult.root), cseResult.named.mapValues(sr0))
  }

  /** Transform the CSE result to SSA form in which each operation take only
   *  names as arguments and each result is assigned a new name. */
  def toSSA(srResult: SR.Result): SSA.Result = {
    import SSA._

    /* Return the names that must be available before this Op can be
     * computed.*/
    def dependencies(op: SR.Op): Set[TermName] = {
      def loop(op: SR.Op): Set[TermName] = {
        import SR._
        op match {
          case Ref(name) => Set(name)
          case Const(_) => Set()
          case Mat(_) => Set()
          case NewMat(_) => Set()
          case Mat_*(left, right) => loop(left) ++ loop(right)
          case Mat_+(left, right) => loop(left) ++ loop(right)
          case Mat_T(matrix) => loop(matrix)
          case Scalar_*(_, right) => loop(right)
          case DGEMM(_, _, a, _, b, _, c) => loop(a) ++ loop(b) ++ loop(c)
          case DGEMM0(_, a, _, b, _) => loop(a) ++ loop(b)
        }
      }
      loop(op)
    }

    /* Returns the namedOps in executable order. */
    def order(namedOps: Map[TermName, SR.Op]): List[(TermName, SR.Op)] = {
      def loop (deps: Map[TermName, Set[TermName]]): List[(TermName, SR.Op)] = {
        if (deps.isEmpty) List()
        else {
          deps.find{ case (name, depNames) => depNames.isEmpty } match {
            case None => throw new RuntimeException("Cyclic dependencies")
            case Some((name, _)) =>
              val rest = (deps - name).mapValues(s => s - name)
              (name, namedOps(name)) :: loop(rest)
          }
        }
      }
      if (namedOps.isEmpty) List()
      else {
        val deps: Map[TermName, Set[TermName]] = namedOps.mapValues(dependencies)
        println("DEPS: " + deps)
        loop(deps)
      }
    }

    /* Convert op to SSA form, resulting list is in reverse order */
    def convert(op: SR.Op): List[Op] = {
      def loop(op: SR.Op): (TermName, List[Op]) = {
        op match {
          case SR.Ref(name) => (name, List())
          case SR.Const(tree) =>
            val name = fresh("ssa_const")
            (name, List(Assign(name, Const(tree))))
          case SR.Mat(tree) =>
            val name = fresh("ssa_mat")
            (name, List(Assign(name, Mat(tree))))
          case SR.NewMat(tree) =>
            val name = fresh("ssa_new_mat")
            (name, List(Assign(name, NewMat(tree))))
          case SR.Mat_*(left, right) =>
            val name = fresh("ssa_mat_*")
            val (leftName, leftOps) = loop(left)
            val (rightName, rightOps) = loop(right)
            (name, Assign(name, Mat_*(leftName, rightName))
                   :: rightOps ::: leftOps)
          case SR.Mat_+(left, right) =>
            val name = fresh("ssa_mat_+")
            val (leftName, leftOps) = loop(left)
            val (rightName, rightOps) = loop(right)
            (name, Assign(name, Mat_+(leftName, rightName))
                   :: rightOps ::: leftOps)
          case SR.Mat_T(matrix) =>
            val name = fresh("ssa_matT")
            val (mName, mOps) = loop(matrix)
            (name, Assign(name, Mat_T(mName)) :: mOps)
          case SR.Scalar_*(left, right) =>
            val name = fresh("ssa_scalar*")
            val (leftName, leftOps) = loop(left)
            val (rightName, rightOps) = loop(right)
            (name, Assign(name, Scalar_*(leftName, rightName))
                   :: rightOps ::: leftOps)
          case SR.DGEMM(SR.Const(alpha), SR.Const(beta), a, SR.Const(at), b, SR.Const(bt), c) =>
            val name = fresh("ssa_dgemm")
            val (aName, aOps) = loop(a)
            val (bName, bOps) = loop(b)
            val (cName, cOps) = loop(c)
            (name, Assign(name, DGEMM(Const(alpha), Const(beta), aName, Const(at), bName, Const(bt), cName))
                   :: aOps ::: bOps ::: cOps)
          case SR.DGEMM0(SR.Const(alpha), a, SR.Const(at), b, SR.Const(bt)) =>
            val name = fresh("ssa_dgemm0")
            val (aName, aOps) = loop(a)
            val (bName, bOps) = loop(b)
            (name, Assign(name, DGEMM0(Const(alpha), aName, Const(at), bName, Const(bt)))
                   :: aOps ::: bOps)
        }
      }
      val (_, ops) = loop(op)
      ops match {
        case List() => ops
        case Assign(_, res) :: rest => (res :: rest)
        case _ => throw new RuntimeException("Conversion error")
      }
    }

    // Convert returns list in reverse order so build the program from
    // the end and then reverse it
    val end = convert(srResult.root)
    val start = order(srResult.named).reverse.flatMap{case (name, cseOp) => {
      convert(cseOp) match {
        case List() => List()
        case op :: ops => Assign(name, op) :: ops
      }
    } }
    Result((end ::: start).reverse)
  }

  // TODO: preserve original names?
  def toTree(op: Unoptimized.Op, alloc: Tree): Tree = {
    import Unoptimized._
    val allocName = fresh("allocator")
    var schedule = q"val $allocName = $alloc" :: List.empty[Tree]
    val scheduleMap = mutable.Map.empty[Op, TermName]
    def loop(op: Op): TermName =
      if (scheduleMap.contains(op))
        scheduleMap(op)
      else
        op match {
          case Mat(tree) =>
            val name = fresh("matrix_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case NewMat(tree) =>
            val name = fresh("new_matrix_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case Const(tree) =>
            val name = fresh("constant_leaf")
            schedule = q"val $name = $tree" :: schedule
            scheduleMap += ((op, name))
            name
          case Mat_*(left, right) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_multiply")
            schedule = q"val $name = $leftname.*($rightname)($allocName)" :: schedule
            scheduleMap += ((op, name))
            name
          case Mat_+(left, right) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_multiply")
            schedule = q"val $name = $leftname.+($rightname)($allocName)" :: schedule
            scheduleMap += ((op, name))
            name
          case Scalar_*(left, right) =>
            val leftname = loop(left)
            val rightname = loop(right)
            val name = fresh("matrix_scalar_multiply")
            schedule = q"val $name = $leftname.*($rightname)($allocName)" :: schedule
            scheduleMap += ((op, name))
            name
          case Mat_T(m) =>
            val transposed = loop(m)
            val name = fresh("matrix_transpose")
            schedule = q"val $name = $transposed.t()($allocName)" :: schedule
            scheduleMap += ((op, name))
            name
            /*
          case DGEMM(alpha, beta, a, at, b, bt, c) =>
            val alphaName = loop(alpha)
            val betaName = loop(beta)
            val aName = loop(a)
            val atName = loop(at)
            val bName = loop(b)
            val btName = loop(bt)
            val cName = loop(c)
            schedule = q"$cName.dgemm($alphaName, $betaName, $aName, $atName, $bName, $btName)" :: schedule
            scheduleMap --= scheduleMap.filter(_._2 == cName).keys
            scheduleMap += ((op, cName))
            cName
            */
        }

    val last = loop(op)
    q"{ ..${schedule.reverse}; $last }"
  }

  /** Return a block executing each tree of the given list in order. */
  def makeBlock(ts: List[Tree]): Tree = {
    ts match {
      case List() => q"{}"
      case t :: rest => q"{..$ts}"

    }
  }

  /** Convert the SSA form into a scala tree. */
  def simpleSchedule(ssaResult: SSA.Result, alloc: Tree): Tree = {
    import SSA._
    def toTree(op: Op): Tree = {
      op match {
        case Assign(name, op) =>
          val t = toTree(op)
          q"val $name = $t"
        case Const(tree) => tree
        case Mat(tree) => tree
        case NewMat(tree) => q"scala.offheap.numeric.DenseMatrix($tree)"
        case Mat_*(left, right) => q"$left.*($right)($alloc)"
        case Mat_+(left, right) => q"$left.+($right)($alloc)"
        case Scalar_*(left, right) => q"$left.*($right)($alloc)"
        case Mat_T(matrix) => q"$matrix.t()(alloc)"
        case DGEMM(Const(alpha), Const(beta), a, Const(at), b, Const(bt), c) =>
          q"($c.copy()).dgemm($alpha, $beta, $a, $at, $b, $bt)"
        case DGEMM0(Const(alpha), a, Const(at), b, Const(bt)) =>
          q"(DenseMatrix.uninit($a.rows, $b.columns)).dgemm($alpha, 0.0, $a, $at, $b, $bt)"
      }
    }

    makeBlock(ssaResult.ops.map(toTree))
  }

  /** Convert the SSA form into a scala tree reusing existing matrices. */
  def schedule(ssaResult: SSA.Result, alloc: Tree): Tree = {
    import SSA._

    /** Return the names used by the given Op. */
    def usedNames(op: Op): Set[TermName] = {
      op match {
        case Assign(name, op) => usedNames(op)
        case Const(tree) => Set()
        case Mat(tree) => Set()
        case NewMat(tree) => Set()
        case Mat_*(left, right) => Set(left, right)
        case Mat_+(left, right) => Set(left, right)
        case Scalar_*(left, right) => Set(left, right)
        case Mat_T(matrix) => Set(matrix)
        case DGEMM(_, _, a, _, b, _, c) => Set(a, b, c)
        case DGEMM0(_, a, _, b, _) => Set(a, b)
      }
    }

    /** For each Op return the Set of name used at this Op and the ones following. */
    def nameUsage(ops: List[Op], prev: List[Set[TermName]]): List[Set[TermName]] = {
      var prevNames = prev match {
        case List() => Set()
        case p :: _ => p
      }
      ops match {
        case List() => prev
        case op :: rest => nameUsage(rest, (prevNames ++ usedNames(op)) :: prev)
      }
    }

    /* Those keep state between calls to toTree. */
    val nameMap: mutable.Map[TermName, TermName] = mutable.Map.empty[TermName, TermName]
    val existingNames: mutable.Set[TermName] = mutable.Set.empty[TermName]
    /** Convert an Op to a scala tree, due to being stateful it must be called
     *  for each Op in program order. */
    def toTree(opsWithNextUsed: (Op, Set[TermName])): Tree = {
      println("toTree: " + opsWithNextUsed._1)
      println("nxtUse: " + opsWithNextUsed._2)
      def rename(name: TermName, into: TermName): Unit = {
        nameMap += ((name, into))
      }

      def getName(name: TermName): TermName = {
        nameMap.getOrElse(name, name)
      }

      def findUnused(nextUsed: Set[TermName], prefer: Set[TermName]): Option[TermName] = {
        println("  findUnused(" + nextUsed + ", " + prefer + ")")
        println("  existing: " + existingNames)
        println("  nextused: " + nextUsed.map(getName))
        val available = existingNames -- (nextUsed.map(getName))
        val prefered = available & prefer
        val res = prefered.headOption.orElse(available.headOption)
        println("    -> " + res)
        res
      }

      val res = opsWithNextUsed._1 match {
        case Assign(name, Const(tree)) => q"val $name = $tree"
        case Assign(name, Mat(tree)) => q"val $name = $tree"
        case Assign(name, NewMat(tree)) => q"val $name = scala.offheap.numeric.DenseMatrix($tree)"
        case Assign(name, Mat_+(left0, right0)) =>
          val left = getName(left0)
          val right = getName(right0)
          findUnused(opsWithNextUsed._2, Set(left, right)) match {
            case None =>
              existingNames += name
              q"val $name = $left.+($right)($alloc)"
            case Some(newName) =>
              if (newName == left) {
                rename(name, newName)
                q"$left.+=($right)"
              } else if (newName == right) {
                rename(name, newName)
                q"$right.+=($left)"
              }
              else {
                // Try to update if size match at runtime?
                existingNames += name
                q"val $name = $left.+($right)($alloc)"
              }
          }
        case Assign(name, Mat_*(left0, right0)) =>
          val left = getName(left0)
          val right = getName(right0)
          findUnused(opsWithNextUsed._2, Set(left, right)) match {
            case None =>
              existingNames += name
              q"val $name = $left.*($right)($alloc)"
            case Some(newName) =>
            if (newName == left || newName == right) {
              // No inplace matrix multiplication
              existingNames += name
              q"val $name = $left.*($right)($alloc)"
            }
            else {
              /* Either a new matrix or the one with newName will be returned
               * at runtime. Since we dont know at compile time, a new name
               * for that result is created. */
              val newName2 = fresh("scheduleMat_*")
              rename(name, newName2)
              existingNames += newName2
              /* Make the old name unavailable since newName2 might alias to it. */
              existingNames -= newName
              q"val $newName2 = $newName._maybeUpdateToMulRes($left, false, $right, false)($alloc)"
            }
          }
        case Assign(name, Scalar_*(left0, right0)) =>
          val left = getName(left0)
          val right = getName(right0)
          findUnused(opsWithNextUsed._2, Set(right)) match {
            case None =>
              existingNames += name
              q"val $name = $left.*($right)($alloc)"
            case Some(newName) =>
              if (newName == right) {
                rename(name, newName)
                q"$right.:*=($left)"
              }
              else {
              /* Same as above: maybe will end up allocating a new matrix at
               * runtime so newName becomes unusable. */
              val newName2 = fresh("scheduleScalar_*")
              rename(name, newName2)
              existingNames += newName2
              existingNames -= newName
              q"val $newName2 = $newName._maybeUpdateToScalarMulRes($left, $right)($alloc)"
            }
          }
        case Assign(name, Mat_T(matrix0)) =>
          val matrix = getName(matrix0)
          findUnused(opsWithNextUsed._2, Set(matrix)) match {
            case None =>
              existingNames += name
              q"val $name = $matrix.t()($alloc)"
            case Some(newName) =>
              // Inplace transposition not supported (yet?)
              if (newName == matrix) {
                existingNames += name
                q"val $name = $matrix.t()($alloc)"
              }
              else {
                // Try to update if size match at runtime?
                existingNames += name
                q"val $name = $matrix.t()($alloc)"
              }
          }
        case Assign(name, DGEMM(Const(alpha), Const(beta), a0, Const(at), b0, Const(bt), c0)) =>
          val a = getName(a0)
          val b = getName(b0)
          val c = getName(c0)
          findUnused(opsWithNextUsed._2, Set(c)) match {
            case Some(newName) if newName == c =>
              rename(name, newName)
              q"$c.dgemm($alpha, $beta, $a, $at, $b, $bt)"
            case _ =>
              existingNames += name
              q"val $name = ($c.copy()).dgemm($alpha, $beta, $a, $at, $b, $bt)"
          }
        case Assign(name, DGEMM0(Const(alpha), a0, Const(at), b0, Const(bt))) =>
          val a = getName(a0)
          val b = getName(b0)
          findUnused(opsWithNextUsed._2, Set()) match {
            case None =>
              existingNames += name
              q"val $name = (DenseMatrix.uninit($a.rows, $b.columns)).dgemm($alpha, 0.0, $a, $at, $b, $bt)"
            case Some(newName) =>
              /* Either a new matrix or the one with newName will be returned
               * at runtime. Since we dont know at compile time, a new name
               * for that result is created. */
              val newName2 = fresh("scheduleDGEMM0")
              rename(name, newName2)
              existingNames += newName2
              existingNames -= newName
              q"val $newName2 = $newName._maybeUpdateToDGEMM0($alpha, $a, $at, $b, $bt)($alloc)"
          }
        case Assign(name, _) =>
          // Necessary because scalac doesn't emmit warnings about unhandled cases.
          throw new RuntimeException("Unhandled case, please add case Assign(name, ...) above")
        /* Those are for the final expression. */
        case Const(tree) => tree
        case Mat(tree) => tree
        case op =>
          val name = fresh("return")
          val tree = toTree((Assign(name, op), opsWithNextUsed._2))
          val newName = getName(name)
          tree match {
            case ValDef(_, _, _, rhs) =>
              rhs
            case _ =>
              q"$tree; $newName"
          }
      }
      println("  -> " + showCode(res))
      res
    }

    var used = nameUsage(ssaResult.ops.reverse, List())
    var usedNext: List[Set[TermName]] = used.tail ::: List(Set.empty[TermName])
    if (usedNext.size != ssaResult.ops.size)
      throw new RuntimeException("Programming error")
    makeBlock(ssaResult.ops.zip(usedNext).map(toTree))
  }

  def opt(alloc: Tree, t: Tree): Tree = {
    val op = toOp(t)
    println(s"parsed op: $op")
    val opCSE = cse(op)
    println(s"after CSE: $opCSE")
    val opSR = sr(opCSE)
    println(s"after SR: $opSR")

    //val optOp = optimise(op)
    //println(s"optimised op: $optOp")
    val optSSA = toSSA(opSR)
    println(s"SSA form: $optSSA")

    //val res = toTree(optOp, alloc)
    //println(s"macro result: ${showCode(res)}")
    //res
    //val res = simpleSchedule(optSSA, alloc)
    val res = schedule(optSSA, alloc)
    println("macro result: " + showCode(res))
    res
  }
}
