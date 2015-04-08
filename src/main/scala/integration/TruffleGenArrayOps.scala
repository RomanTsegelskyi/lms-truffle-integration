package integration

import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.nodes.Node.Child

import scala.annotation.meta.field
import scala.virtualization.lms.common._

/**
  */
trait TruffleGenArrayOps extends TruffleBaseGen {
  val IR: ArrayOpsExp

  import IR._

  override def genNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ArrayNew(n) => createDefinition(sym, TArrayNew(n))
    case ArrayApply(x, n) => createDefinition(sym, TArrayApply(x, n))
    case ArrayUpdate(x, n, y) => createDefinition(sym, TArrayUpdate(x, n, y))
    case Reflect(s, u, effects) =>
      genNode(sym, s)
    case Reify(s, u, effects) =>
    case _ => super.genNode(sym, rhs)
  }

  case class TArrayApply[T](@(Child@field) arr: ExpNode[Array[T]], @(Child@field) x: ExpNode[Int]) extends DefNode[T] {
    def execute(frame: VirtualFrame) = {
      val index = x.execute(frame)
      val res = arr.execute(frame)(index)
      res
    }
  }

  case class TArrayUpdate[T](@(Child@field) arr: ExpNode[Array[T]],
                            @(Child@field) index: ExpNode[Int],
                            @(Child@field) element: ExpNode[T]) extends DefNode[Unit] {
    def execute(frame: VirtualFrame) = {
      val array = arr.execute(frame)
      array(index.execute(frame)) = element.execute(frame)
    }
  }

  case class TArrayNew[T: Manifest](@(Child@field) size: ExpNode[Int]) extends DefNode[Array[T]] {
    def execute(frame: VirtualFrame) = {
      val s = size.execute(frame);
      new Array[T](s);
    }
  }
}

