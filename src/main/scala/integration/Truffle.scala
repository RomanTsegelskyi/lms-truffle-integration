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

trait TruffleCodeGenPkg extends TruffleGenNumericOps { val IR: ScalaOpsPkgExp }

trait TruffleGenNumericOps extends TruffleGen {
  val IR: NumericOpsExp
  import IR._

  def genNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.genNode(sym, rhs)
  }
}

trait TruffleGen extends BlockTraversal with Config {
  // truffle interface
  val IR: Expressions
  import IR._
  trait Typ[T] {
    def slotKind: FrameSlotKind
  }

  type Rep[+T] = Exp[T]

  // truffle interface
  var runtime: TruffleRuntime = _
  var frameDescriptor: FrameDescriptor = _

  var localDefs: ArrayBuffer[Stm] = null

  var varCount = 0

  implicit def lift[T: Typ: Manifest](x: T): Exp[T] = Const(x)

  override def createDefinition[T: Typ](v: Sym[T], d: Def[T]) = { localDefs += AssignNode(v.slot, d); v }

  def getArg[T: Typ](index: Int): Stm = {
    val x = createDefinition(fresh, GetArg[T](index))
    x
  }

  case class GetArg[@specialized T: Typ](index: Int) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      val args = frame.getArguments()(0).asInstanceOf[Array[Object]];
      args(index).asInstanceOf[T]
    }
  }

  def reifyBlock[T: Typ](d: => Exp[T]): BlockNode[T] = {
    val save = localDefs
    localDefs = new ArrayBuffer
    val res = d
    val stms = localDefs.toArray
    localDefs = save
    BlockNode(stms, res)
  }

  def lms[T: Typ, U: Typ](f: Rep[T] => Rep[U]) = new (T => U) {
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor();
        val t = reifyBlock(f(getArg[T](0)));
        new LMSRootNode(frameDescriptor, BlockNode(t._2, t._1));
      }
    }
    val target = runtime.createCallTarget(rootNode)

    override def apply(x: T) = {
      val result = target.call(Array(x.asInstanceOf[AnyRef]));
      result.asInstanceOf[U]
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

  def genAST[A: Manifest](args: List[Sym[_]], body: Block[A]): LMSRootNode[A] = {
    emitBlock(body)
  }

  case class AssignNode[@specialized T: Typ](slot: FrameSlot, @(Child @field) d: DefNode[T]) extends StmNode {
    val kind = slot.getKind
    def execute(frame: VirtualFrame): Unit = {
      //      println("Slot = " + slot)
      //      println("Kind = " + kind)
      val e = d.execute(frame)
      //      println("executed = " + e)
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

}
