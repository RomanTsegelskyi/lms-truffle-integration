package integration

import scala.virtualization.lms.common._
import org.scalatest.FunSuite
import java.io.PrintWriter

class TruffleTest extends FunSuite {
  test("power1") {
    val driver = new Power1 with TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val sum_c = {
      import driver._
      val sum22 = (x: Rep[Int]) => 11 + x + 22 + 21
      runtimegen.genAST(sum22)
    }
    println(sum_c(2));
  }
}