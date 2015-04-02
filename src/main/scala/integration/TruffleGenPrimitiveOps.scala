package integration

import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.nodes.Node.Child

import scala.annotation.meta.field
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._

/**
 */
trait TruffleGenPrimitiveOps extends TruffleBaseGen {
  val IR: PrimitiveOpsExp
  import IR._

  override def genNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case IntPlus(a, b) => createDefinition(sym, IntPlusNode(a, b))
    case IntMinus(a, b) => createDefinition(sym, IntMinusNode(a, b))
    case IntTimes(a, b) => createDefinition(sym, IntTimesNode(a, b))
    case IntDivide(a, b) => createDefinition(sym, IntDivNode(a, b))
    case _ => super.genNode(sym, rhs)
  }

  case class IntPlusNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) + y.execute(frame)
    }
  }

  case class IntMinusNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) - y.execute(frame)
    }
  }
  case class IntTimesNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) * y.execute(frame)
    }
  }
  case class IntModNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) % y.execute(frame)
    }
  }
  case class IntDivNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Int] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) / y.execute(frame)
    }
  }
  case class IntEqualNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) == y.execute(frame)
    }
  }
  case class IntNotEqualNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) != y.execute(frame)
    }
  }
  case class IntLessNode(@(Child @field) x: ExpNode[Int], @(Child @field) y: ExpNode[Int]) extends DefNode[Boolean] {
    def execute(frame: VirtualFrame) = {
      x.execute(frame) < y.execute(frame)
    }
  }
}

