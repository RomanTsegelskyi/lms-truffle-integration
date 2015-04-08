package integration

import org.scalatest.FunSuite

import scala.virtualization.lms.common.LiftNumeric

class ArrayTest extends FunSuite{
  test("simple access") {
    val driver = new  TruffleOpsPkgExp with LiftNumeric { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val array_c= {
      import driver._
      val fun = (x: Rep[Array[Int]]) => x(0) + x(1)
      runtimegen.genAST(fun)
    }

    var arr = Array(20, 22)
    val result = array_c(arr)
    println(array_c.rootNode.block.toString)
    assert(result === 42)
  }

  test("simple assignment") {
    val driver = new  TruffleOpsPkgExp with LiftNumeric { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val array_c = {
      import driver._
      val fun = (x: Rep[Array[Int]]) => {
        x(0) = 1
        x(0)
      }
      runtimegen.genAST(fun)
    }

    var arr = Array(20, 22)
    println(array_c.rootNode.block.toString)
    val result = array_c(arr)
    assert(result === 1)
  }

}