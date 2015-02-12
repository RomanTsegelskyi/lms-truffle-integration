package integration

import scala.virtualization.lms.common._
import org.scalatest.FunSuite
import java.io.PrintWriter

class TruffleTest extends FunSuite {
  test("power1") {
    val driver = new Power1 with TruffleOpsPkg { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val power8c = {
      import driver._
      val power4 = (x: Rep[Int]) => power(x, 4)
      runtimegen.genAST(power4)
    }
    println(power8c(2));
  }
}