package integration

import org.scalatest.FunSuite

import scala.virtualization.lms.common.LiftNumeric

trait Power1 extends TruffleOpsPkg with LiftNumeric {
  def power(x: Rep[Int], y:Int): Rep[Int] =
    if (y == 0) 1 else x * power(x, y - 1)
}

trait Power2 extends TruffleOpsPkg with LiftNumeric {
  def power(b: Rep[Int], x: Int): Rep[Int] = {
    if (x == 0) 1
    else if ((x % 2) == 0) { power(b, x / 2) * power(b, x / 2) }
    else b * power(b, x - 1)
  }
}

class PowerTest extends FunSuite {
  test("PowerTest 1") {
    val driver = new  Power1 with TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val power8_c= {
      import driver._
      val fun = (x: Rep[Int]) => power(x, 8)
      runtimegen.genAST(fun)
    }
    val result = power8_c(2)
    assert(result === 256)
  }

  test("PowerTest 2") {
    val driver = new  Power2 with TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val power8_c= {
      import driver._
      val fun = (x: Rep[Int]) => power(x, 8)
      runtimegen.genAST(fun)
    }
    val result = power8_c(2)
    assert(result === 256)
  }
}