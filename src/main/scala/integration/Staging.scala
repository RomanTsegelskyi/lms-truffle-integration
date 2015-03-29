package integration

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import math._

trait Power1 extends TruffleOpsPkg with LiftNumeric {

  def power(b: Rep[Int]): Rep[Int] =
    b + 22
}
