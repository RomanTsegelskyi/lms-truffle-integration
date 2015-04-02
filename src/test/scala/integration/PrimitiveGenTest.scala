package integration

import org.scalatest.FunSuite
import org.scalatest._

class PrimitiveGenTest extends FunSuite {
  test("PrimitiveGenTest 1") {
    val driver = new TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val fun_c = {
      import driver._
      val fun = (x: Rep[Int]) => 2 * x + 10
      runtimegen.genAST(fun)
    }
    val result = fun_c(2)
    assert(result === 14)
  }


  test("PrimitiveGenTest 2") {
    val driver = new TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val fun_c = {
      import driver._
      val fun = (x: Rep[Int]) => 2 * (x/2 + 10)
      runtimegen.genAST(fun)
    }
    val result = fun_c(2)
    assert(result === 22)
  }

  test("PrimitiveGenTest 3") {
    val driver = new TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val fun_c = {
      import driver._
      val fun = (x: Rep[Int]) => 2 * (x + 1) + x
      runtimegen.genAST(fun)
    }
    val result = fun_c(2)
    assert(result === 8)
  }
}