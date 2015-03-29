package integration

import com.oracle.truffle.api.TruffleRuntime
import com.oracle.truffle.api.frame.{FrameSlot, VirtualFrame, FrameDescriptor, FrameSlotKind}
import com.oracle.truffle.api.nodes.Node.{Children, Child}
import com.oracle.truffle.api.nodes.{ExplodeLoop, Node, RootNode}

import scala.annotation.meta.field
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.internal._


trait TruffleGen extends NestedBlockTraversal with Config {
  val IR: Expressions with Effects
  import IR._

  trait Typ[T] {
    def slotKind: FrameSlotKind
  }

  // truffle interface
  var runtime: TruffleRuntime = _
  var frameDescriptor: FrameDescriptor = _

  var localDefs: ArrayBuffer[StmNode] = new ArrayBuffer[StmNode]();

  var varCount = 0

  implicit def lift[T:Manifest](x: T): Exp[T] = Const(x)

  def createDefinition[T](v: SymNode[T], d: DefNode[T]) = { localDefs += AssignNode(v.slot, d); v }

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }

  override def traverseStmsInBlock[A](stms: List[Stm]) = {
    stms foreach traverseStm
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => {
      println("Symbol id = " + sym.id);
      println("RHS = " + rhs);
      localDefs += genNode(sym, rhs);
    };
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }


  def genAST[T:Manifest, U:Manifest](f: Exp[T] => Exp[U]) = new (T => U) {
    println("In gen")
    val rootNode = {
      val saveC = varCount
      val saveD = frameDescriptor
      try {
        varCount = 0
        frameDescriptor = new FrameDescriptor()
        val s = fresh[T]
        val t = reifyBlock(f(s))
        traverseBlock(t)
        println(localDefs)
        // we need to traverse block here and generate an AST
        //        new LMSRootNode(frameDescriptor, null);
      } finally {
      }
    }
    //    val target = runtime.createCallTarget(rootNode)

    override def apply(x: T) = {
      //      val result = target.call(Array(x.asInstanceOf[AnyRef]));
      println("Before as instance")
      1.asInstanceOf[U]

    }
  }

  def genNode(sym: Sym[Any], rhs: Def[Any]): StmNode = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  //  def getArg[T: Typ](index: Int): Exp[T] = {
  //    val x = createDefinition(fresh, GetArg[T](index))
  //    x
  //  }

  case class GetArg[@specialized T](index: Int) extends Def[T] {
    def execute(frame: VirtualFrame) = {
      val args = frame.getArguments()(0).asInstanceOf[Array[Object]];
      args(index).asInstanceOf[T]
    }
  }

  class LMSRootNode[@specialized T](desc: FrameDescriptor, @(Child @field) val block: BlockNode[T]) extends RootNode(null, desc) {
    override def execute(frame: VirtualFrame): AnyRef = block.execute(frame).asInstanceOf[AnyRef]
  }

  def genExpNode[T](stmt: Exp[T]) = {
    stmt match {
      case Const(a) => new ConstNode[T](a.asInstanceOf[T]);
      case Sym(a) => new SymNode(frameDescriptor.addFrameSlot(s"x$a.id", implicitly[Typ[T]].slotKind));
    }
  }


  case class BlockNode[T](@(Children @field) stms: Array[StmNode], res: ExpNode[T]) extends Node {
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

  case class AssignNode[@specialized T](slot: FrameSlot, @(Child @field) d: DefNode[T]) extends StmNode {
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

  case class ConstNode[@specialized T](value: T) extends ExpNode[T] {
    def execute(frame: VirtualFrame): T = value
  }

  case class SymNode[@specialized T](val slot: FrameSlot) extends ExpNode[T] {
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
