package integration

import com.oracle.truffle.api.TruffleRuntime
import com.oracle.truffle.api.frame.{FrameSlot, VirtualFrame, FrameDescriptor, FrameSlotKind}
import com.oracle.truffle.api.nodes.Node.{Children, Child}
import com.oracle.truffle.api.nodes.{ExplodeLoop, Node, RootNode}

import scala.annotation.meta.field
import scala.virtualization.lms.common._

trait TruffleGenPkg extends TruffleGenPrimitiveOps { val IR: TruffleOpsPkgExp }

trait TruffleOpsPkgExp extends TruffleOpsPkg
with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with ListOpsExp
with SeqOpsExp with DSLOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp with ArrayBufferOpsExp


trait TruffleOpsPkg extends Base
with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
with Equal with IfThenElse with Variables with While with TupleOps with ListOps
with SeqOps with MathOps with CastingOps with SetOps with ObjectOps with ArrayBufferOps

trait TruffleGenPrimitiveOps extends TruffleGen {
  val IR: PrimitiveOpsExp
  import IR._

  case class IntPlusNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }

  override def genNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IntPlus(a, b) => {
      val x = createDefinition(genSymNode(sym), IntPlusNode(genExpNode(a), genExpNode(b)))
    }
    case _ => super.genNode(sym, rhs)
  }
}
