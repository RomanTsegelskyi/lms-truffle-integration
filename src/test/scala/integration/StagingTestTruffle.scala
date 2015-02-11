package integration

import scala.virtualization.lms.common._
import org.scalatest.FunSuite
import java.io.PrintWriter

class StagingTest2 extends FunSuite {
  test("power1") {
    val driver = new Power1 with TruffleOpsPkgExp { self =>
      val runtimegen = new TruffleGenPkg { val IR: self.type = self }
    }

    val power8c = {
      import driver._

      runtimegen.lms { x: Rep[Int] =>

        def power(x: runtimegen.Rep[Int], y: Int): runtimegen.Rep[Int] = {
          if (y == 0) runtimegen.lift(1) else x * power(x, y - 1)
        }
        power(x, 6)
      }
    }
    println(power8c(2));
  }
}