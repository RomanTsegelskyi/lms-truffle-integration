package integration

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.reflect.SourceContext
import com.oracle.truffle.api._
import com.oracle.truffle.api.frame._
import com.oracle.truffle.api.nodes._
import com.oracle.truffle.api.nodes.Node._
import scala.collection.mutable.ArrayBuffer
import scala.annotation.meta.field

trait TruffleGenPkg extends TruffleGenNumericOps { val IR: TruffleOpsPkg }

trait TruffleOpsPkg extends Base
  with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
  with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
  with Equal with IfThenElse with Variables with While with TupleOps with ListOps
  with SeqOps with MathOps with CastingOps with SetOps with ObjectOps with ArrayBufferOps

trait TruffleGenNumericOps extends TruffleGen {
  val IR: NumericOpsExp
  import IR._

  case class IntPlus(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends Def[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }

  override def genNode(sym: Sym[Any], rhs: DefNode[Any]) = rhs match {
    case NumericPlus(a, b) => reflect(IntPlus(a, b))
    case _ => super.genNode(sym, rhs)
  }
}

trait TruffleGen extends NestedBlockTraversal with Config with Base {
  val IR: Expressions
  import IR._

  trait Typ[T] {
    def slotKind: FrameSlotKind
  }

  def reflect[T: Typ](d: Def[T]): Exp[T] = {
    val x = createDefinition(fresh, d)
    x
  }

  // truffle interface
  var runtime: TruffleRuntime = _
  var frameDescriptor: FrameDescriptor = _

  var localDefs: ArrayBuffer[Stm] = null

  var varCount = 0

  implicit def lift[T: Typ: Manifest](x: T): Exp[T] = Const(x)

  override def createDefinition[T: Typ](v: SymNode[T], d: DefNode[T]) = { localDefs += AssignNode(v.slot, d); v }

  def genAST[T: Typ, U: Typ](f: Exp[T] => Exp[U]) = new (T => U) {
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor()
        val t = reifyBlock(f(getArg[T](0)))
        // we need to traverse block here and generate an AST
        new LMSRootNode(frameDescriptor, t);
      } finally {
      }
    }
    val target = runtime.createCallTarget(rootNode)

    override def apply(x: T) = {
      val result = target.call(Array(x.asInstanceOf[AnyRef]));
      result.asInstanceOf[U]
    }
  }

  def genNode(sym: Sym[Any], rhs: DefNode[Any]) = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  def getArg[T: Typ](index: Int): Exp[T] = {
    val x = createDefinition(fresh, GetArg[T](index))
    x
  }

  case class GetArg[@specialized T: Typ](index: Int) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      val args = frame.getArguments()(0).asInstanceOf[Array[Object]];
      args(index).asInstanceOf[T]
    }
  }

  class LMSRootNode[@specialized T](desc: FrameDescriptor, @(Child @field) val block: BlockNode[T]) extends RootNode(null, desc) {
    override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
  }

  case class BlockNode[T: Typ](@(Children @field) stms: Array[StmNode], res: ExpNode[T]) extends Node {
    @ExplodeLoop def execute(frame: VirtualFrame): T = {
      var i = 0
      while (i < stms.length) {
        //        println(i)
        stms(i).execute(frame)
        i += 1
      }
      //stms.foreach(_.execute(frame))
      res.execute(frame)
    }
    override def toString = stms.map(_.toString).mkString("\n")
  }

  abstract class BaseNode extends Node with Product {
    def prettyString = productPrefix + "(" + ((0 until productArity) map productElement mkString ",") + ")"
    override def toString = prettyString
  }

  trait ExpNode[@specialized +T] extends BaseNode {
    def execute(frame: VirtualFrame): T
  }

  trait DefNode[@specialized T] extends BaseNode {
    def execute(frame: VirtualFrame): T
  }

  trait StmNode extends BaseNode {
    def execute(frame: VirtualFrame): Unit
  }

  case class AssignNode[@specialized T: Typ](slot: FrameSlot, @(Child @field) d: DefNode[T]) extends StmNode {
    val kind = slot.getKind
    def execute(frame: VirtualFrame): Unit = {
      val e = d.execute(frame)
      kind match {
        case FrameSlotKind.Int =>
          frame.setInt(slot, e.asInstanceOf[Int])
        case FrameSlotKind.Boolean =>
          frame.setBoolean(slot, e.asInstanceOf[Boolean])
        case FrameSlotKind.Long =>
          frame.setLong(slot, e.asInstanceOf[Long])
        case FrameSlotKind.Double =>
          frame.setDouble(slot, e.asInstanceOf[Double])
        case FrameSlotKind.Float =>
          frame.setFloat(slot, e.asInstanceOf[Float])
        case FrameSlotKind.Byte =>
          frame.setByte(slot, e.asInstanceOf[Byte])
        case _ =>
          frame.setObject(slot, e.asInstanceOf[T])
      }
    }
  }

  case class SymNode[@specialized T:Typ](val slot: FrameSlot) extends ExpNode[T] {
    val kind = slot.getKind
    def execute(frame: VirtualFrame): T = {
      kind match {
        case FrameSlotKind.Int =>
          frame.getInt(slot).asInstanceOf[T]
        case FrameSlotKind.Boolean =>
          frame.getBoolean(slot).asInstanceOf[T]
        case FrameSlotKind.Long =>
          frame.getLong(slot).asInstanceOf[T]
        case FrameSlotKind.Double =>
          frame.getDouble(slot).asInstanceOf[T]
        case FrameSlotKind.Float =>
          frame.getFloat(slot).asInstanceOf[T]
        case FrameSlotKind.Byte =>
          frame.getByte(slot).asInstanceOf[T]
        case _ =>
          frame.getObject(slot).asInstanceOf[T]
      }
    }
  }

}
