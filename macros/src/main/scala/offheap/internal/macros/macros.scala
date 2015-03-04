package offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Definitions {
  val c: blackbox.Context
  import c.universe._
  import c.universe.rootMirror._

  val MethodModule  = staticModule("offheap.internal.Method")
  val TypeTagModule = staticModule("offheap.internal.TypeTag")
  val PoolModule    = staticModule("offheap.Pool")

  val StringBuilderClass = staticClass("scala.collection.mutable.StringBuilder")
  val RegionClass        = staticClass("offheap.Region")
  val RefClass           = staticClass("offheap.Ref")
  val MemoryClass        = staticClass("offheap.Memory")
  val OffheapClass       = staticClass("offheap.internal.annot.offheap")
  val TagClass           = staticClass("offheap.internal.annot.Tag")
  val LayoutClass        = staticClass("offheap.internal.annot.Layout")
}

trait Common extends Definitions {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)

  def debug[T](header: String)(f: => T): T = {
    val res = f
    println(s"$header = $res")
    res
  }

  def fresh(pre: String): TermName = TermName(c.freshName(pre))

  def freshVal(pre: String, tpe: Type, value: Tree): ValDef = {
    import c.internal._, c.internal.decorators._
    val name = fresh(pre)
    val sym = enclosingOwner.newTermSymbol(name).setInfo(tpe)
    val vd = valDef(sym, value)
    vd
  }

  object Primitive {
    def unapply(tpe: Type): Boolean = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive && sym != UnitClass => true
      case _                                                       => false
    }
  }

  object Allocatable {
    def unapply(tpe: Type): Boolean = tpe match {
      case Primitive() | ClassOf(_) => true
      case _                        => false
    }
  }

  case class Field(name: String, tpe: Type)

  class LayoutAnnotatedClass(val classSym: Symbol) {
    def is(sym: Symbol): Boolean =
      sym.annotations.exists(_.tpe.typeSymbol == classSym)
    def unapply(tpe: Type): Option[List[Field]] = unapply(tpe.widen.typeSymbol)
    def unapply(sym: Symbol): Option[List[Field]] = sym match {
      case sym: ClassSymbol if this.is(sym) =>
        val q"new $_(new $_(..$descriptors))" = sym.annotations.collectFirst {
          case ann if ann.tpe.typeSymbol == classSym => ann
        }.get.tree
        Some(descriptors.map { case q"(${name: String}, new $_[$tpt]())" =>
          Field(name, tpt.tpe)
        })
      case _ => None
    }
  }

  object ClassOf extends LayoutAnnotatedClass(OffheapClass)

  def read(addr: Tree, tpe: Type, memory: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val getT = TermName(s"get$tpe")
      q"$memory.$getT($addr)"
    case BooleanTpe =>
      q"$memory.getByte($addr) != ${Literal(Constant(0.toByte))}"
    case ClassOf(_) =>
      val companion = tpe.typeSymbol.companion
      q"$companion.fromRef($memory.getRef($addr))"
  }

  def write(addr: Tree, tpe: Type, value: Tree, memory: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val putT = TermName(s"put$tpe")
      q"$memory.$putT($addr, $value)"
    case BooleanTpe =>
      q"""
        $memory.putByte($addr,
                        if ($value) ${Literal(Constant(1.toByte))}
                        else ${Literal(Constant(0.toByte))})
      """
    case ClassOf(_) =>
      val companion = tpe.typeSymbol.companion
      q"$memory.putRef($addr, $companion.toRef($value))"
  }

  /*def sizeof(tpe: Type): Int = tpe match {
    case ByteTpe  | BooleanTpe             => 1
    case ShortTpe | CharTpe                => 2
    case IntTpe   | FloatTpe               => 4
    case LongTpe  | DoubleTpe              => 8
    case tpe if ClassOf.is(tpe.typeSymbol) => 16
  }*/

  // TODO: handle non-function literal cases
  def appSubs(f: Tree, argValue: Tree, subs: Tree => Tree) = f match {
    case q"($param => $body)" =>
      import c.internal._, c.internal.decorators._
      val q"$_ val $_: $argTpt = $_" = param
      changeOwner(body, f.symbol, enclosingOwner)
      val (arg, argDef) = argValue match {
        case refTree: RefTree
          if refTree.symbol.isTerm
          && refTree.symbol.asTerm.isStable =>
          (refTree, q"")
        case _ =>
          val vd = freshVal("arg", argTpt.tpe, argValue)
          (q"${vd.symbol}", vd)
      }
      val transformedBody = typingTransform(body) { (tree, api) =>
        tree match {
          case id: Ident if id.symbol == param.symbol =>
            api.typecheck(subs(q"$arg"))
          case _ =>
            api.default(tree)
        }
      }
      q"..$argDef; $transformedBody"
    case _             =>
      q"$f($argValue)"
  }

  def app(f: Tree, argValue: Tree) =
    appSubs(f, argValue, identity)

  def stabilized(tree: Tree)(f: Tree => Tree) = tree match {
    case q"${refTree: RefTree}"
      if refTree.symbol.isTerm
      && refTree.symbol.asTerm.isStable =>
      f(refTree)
    case _ =>
      if (tree.tpe == null) {
        val stable = fresh("stable")
        q"val $stable = $tree; ${f(q"$stable")}"
      } else {
        val stable = freshVal("stable", tree.tpe, tree)
        val fapp = f(q"${stable.symbol}")
        q"$stable; $fapp"
      }
  }

  def paramTpe(f: Tree) = f.tpe.typeArgs.head

  def ensureAllocatable(T: Type): Unit =  T match {
    case Allocatable() => ()
    case _             => abort(s"$T is not fixed sized allocatable object")
  }
}

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._
  import Flag._

  def layout(fields: List[OField]): Tree = {
    val tuples = fields.map { f =>
      q"(${f.name.toString}, new $TagClass[${f.tpt}]())"
    }
    q"new $LayoutClass(..$tuples)"
  }

  case class OField(name: TermName, tpt: Tree, default: Tree,
                    isMutable: Boolean, isCtorField: Boolean)

  // TODO: handle generics
  // TODO: handle implicit parameters
  // TODO: handle existing companions
  // TODO: modifiers propagation
  // TODO: hygienic reference to class type from companion?
  def offheapAnnotation(annottees: Tree*): Tree = annottees match {
    case q"""
        $rawMods class $name[..$rawTargs](..$rawArgs) extends ..$rawParents { ..$rawStats }
      """ :: Nil =>
      // Process and validate existing members
      val fields = {
        def checkMods(mods: Modifiers) =
          if (mods.hasFlag(LAZY)) abort("lazy vals are not supported")
        val argFields = rawArgs.collect {
          case ValDef(mods, name, tpt, default) =>
            checkMods(mods)
            OField(name, tpt, default,
                   isMutable = mods.hasFlag(MUTABLE),
                   isCtorField = true)
        }
        val bodyFields = rawStats.collect {
          case ValDef(mods, name, tpt, _) =>
            checkMods(mods)
            OField(name, tpt, EmptyTree,
                   isMutable = mods.hasFlag(MUTABLE),
                   isCtorField = false)
        }
        argFields ++ bodyFields
      }
      val init = rawStats.collect {
        case t if t.isTerm             => t
        case ValDef(_, name, _, value) => ???
      }
      val traits = rawParents.map {
        case q"${tq"$ref[..$targs]"}(...$args)" =>
          if (args.nonEmpty || targs.nonEmpty)
            abort("offheap classes can only inherit from offheap traits")
          ref
      }
      val methods = rawStats.collect { case t: DefDef => t }
      val types = rawStats.collect { case t: TypeDef => t }
      // Generate additional members
      val ref = TermName("$ref")
      val accessors = fields.flatMap { f =>
        val accessor = q"""
          def ${f.name}: ${f.tpt} =
            $MethodModule.accessor[$name, ${f.tpt}]($ref, ${f.name.toString})
        """
        val assignerName = TermName(f.name.toString + "_$eq")
        val value = fresh("value")
        val assigner = q"""
          def $assignerName($value: ${f.tpt}): Unit =
            $MethodModule.assigner[$name, ${f.tpt}]($ref, ${f.name.toString}, $value)
        """
        if (!f.isMutable) accessor :: Nil
        else accessor :: assigner :: Nil
      }
      val argNames = fields.collect { case f if f.isCtorField => f.name }
      val memory = fresh("memory")
      val instance = fresh("instance")
      val scrutinee = fresh("scrutinee")
      val caseClassSupport =
        if (!rawMods.hasFlag(Flag.CASE)) q""
        else {
          val _ns = argNames.zipWithIndex.map {
            case (argName, i) =>
              val _n = TermName("_" + (i + 1))
              q"def ${_n} = this.$argName"
          }
          val copyArgs = fields.collect { case f if f.isCtorField =>
            q"val ${f.name}: ${f.tpt} = this.${f.name}"
          }
          q"""
            def isEmpty  = ???
            def nonEmpty = ???
            def get      = this
            ..${_ns}
            def copy(..$copyArgs)(implicit $memory: $MemoryClass): $name =
              $MethodModule.copy[$name]($memory, ..$argNames)
            override def toString(): $StringClass =
              $MethodModule.toString[$name]
          """
        }
      // Wrap everything into a nice shiny package
      val args = fields.collect { case f if f.isCtorField =>
        q"val ${f.name}: ${f.tpt} = ${f.default}"
      }
      q"""
        @$OffheapClass(${layout(fields)}) final class $name private(
          private val $ref: $RefClass
        ) extends $AnyValClass {
          def $$initialize$$ = { ..$init }
          ..$accessors
          ..$methods
          ..$types
          ..$caseClassSupport
        }
        object ${name.toTermName} {
          def apply(..$args)(implicit $memory: $MemoryClass): $name =
            $MethodModule.allocator[$name]($memory, ..$argNames)
          def unapply($scrutinee: $name): $name = $scrutinee
          val empty: $name = null.asInstanceOf[$name]
          def fromRef($ref: $RefClass): $name =
            new $name($ref)
          def toRef($instance: $name): $RefClass =
            $instance.$ref
        }
      """
  }
}

class Region(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def open(pool: Tree) = q"???"

  def apply[T: WeakTypeTag](f: Tree)(pool: Tree) = {
    val r = freshVal("r", tpe = RegionClass.toType, value = open(pool))
    val res = fresh("res")
    val body = app(f, q"${r.symbol}")
    q"""
      $r
      val $res =
        try $body
        finally ${r.symbol}.close()
      $res
    """
  }
}

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def throwNullRef = q"throw new _root_.java.lang.NullPointerException"

  def accessor[C: WeakTypeTag, T: WeakTypeTag](ref: Tree, name: Tree): Tree = {
    val C = wt[C]
    val ClassOf(fields) = C
    ensureAllocatable(wt[C])
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        val mem = q"$ref.memory"
        val tpes = fields.takeWhile(_ ne f).map(_.tpe)
        val offset = q"$mem.sizeof[(..$tpes)]"
        read(q"$mem.offset($ref, $offset)", f.tpe, mem)
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def assigner[C: WeakTypeTag, T: WeakTypeTag](ref: Tree, name: Tree, value: Tree) = {
    val C = wt[C]
    val ClassOf(fields) = C
    ensureAllocatable(wt[C])
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        val mem = q"$ref.memory"
        val tpes = fields.takeWhile(_ ne f).map(_.tpe)
        val offset = q"$mem.sizeof[(..$tpes)]"
        write(q"$mem.offset($ref, $offset)", f.tpe, value, mem)
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def allocator[C: WeakTypeTag](memory: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val ClassOf(fields) = C.typeSymbol
    val addr = fresh("addr")
    val writes = fields.zip(args).map { case (f, arg) =>
      val tpes = fields.takeWhile(_ ne f).map(_.tpe)
      val offset = q"$memory.sizeof[(..$tpes)]"
      write(q"$memory.offset($addr, $offset)", f.tpe, arg, memory)
    }
    q"""
      val $addr: $memory.Addr = $memory.allocate($memory.sizeof[$C])
      ..$writes
      new $C($memory.ref($addr))
    """
  }

  def copy[C](memory: Tree, args: Tree*): Tree = q"???"

  def toString[C]: Tree = q"???"
}


class Memory(val c: blackbox.Context) extends Common {
  import c.universe._

  def sizeOf32[T: WeakTypeTag] = q"???"
  def sizeOf64[T: WeakTypeTag] = q"???"
}
