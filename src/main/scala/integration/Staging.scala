package integration

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import math._

trait Power1 extends ScalaOpsPkg with LiftNumeric {

  def power(b: Rep[Int], x: Int): Rep[Int] = 
    if (x == 0) 1 else b * power(b, x - 1)

}

trait Power2 extends ScalaOpsPkg with LiftNumeric {

  def power(b: Rep[Double], x: Int): Rep[Double] = 
    if (x == 0) 1.0
    else if ((x % 2) == 0) { power(b, x/2) * power(b, x/2) }
    else b * power(b, x - 1)
}


trait FFT extends ScalaOpsPkg with LiftNumeric with Trig {
  
  def omega(k: Int, N: Int): Complex = {
    val kth = -2.0 * k * math.Pi / N
    Complex(cos(kth), sin(kth))
  }

  case class Complex(re: Rep[Double], im: Rep[Double]) {
    def +(that: Complex) = Complex(this.re + that.re, this.im + that.im)
    def -(that: Complex) = Complex(this.re - that.re, this.im - that.im)
    def *(that: Complex) = Complex(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  }

  def splitEvenOdd[T](xs: List[T]): (List[T], List[T]) = (xs: @unchecked) match {
    case e :: o :: xt =>
      val (es, os) = splitEvenOdd(xt)
      ((e :: es), (o :: os))
    case Nil => (Nil, Nil)
    // cases?
  } 


  def mergeEvenOdd[T](even: List[T], odd: List[T]): List[T] = ((even, odd): @unchecked) match {
    case (Nil, Nil) =>
      Nil
    case ((e :: es), (o :: os)) =>
      e :: (o :: mergeEvenOdd(es, os))
    // cases?
  }

  def fft(xs: List[Complex]): List[Complex] = xs match {
    case (x :: Nil) => xs
    case _ =>
      val N = xs.length // assume it's a power of two
      val (even0, odd0) = splitEvenOdd(xs)
      val (even1, odd1) = (fft(even0), fft(odd0))
      val (even2, odd2) = (even1 zip odd1 zipWithIndex) map {
        case ((x, y), k) =>
          val z = omega(k, N) * y
          (x + z, x - z)
      } unzip;
      even2 ::: odd2
  }

}

trait Trig extends Base {

  def sin(x: Rep[Double]): Rep[Double]
  def cos(x: Rep[Double]): Rep[Double]

}

trait TrigExp extends Trig with BaseExp {

  case class Sin(x: Exp[Double]) extends Def[Double]
  case class Cos(x: Exp[Double]) extends Def[Double]

  def sin(x: Exp[Double]) = Sin(x)
  def cos(x: Exp[Double]) = Cos(x)
}

trait TrigExpOpt extends TrigExp {

  override def sin(x: Exp[Double]) = x match {
    case Const(x) => unit(math.sin(x))
    case _ => super.sin(x)
  }
  
  override def cos(x: Exp[Double]) = x match {
    case Const(x) => unit(math.cos(x))
    case _ => super.cos(x)
  }

}

trait TrigExpOptFFT extends TrigExpOpt {
   val sin_values = Map(-2*Pi -> 0.0, -3.0/2*Pi -> 1.0, -Pi -> 0.0, -1.0/2*Pi -> -1.0, 0.0 -> 0.0, 1.0/2*Pi -> 1.0, Pi -> 0.0, 3.0/2*Pi -> -1.0, 2*Pi -> 0.0)
   val cos_values = Map(-2*Pi -> 1.0, -3.0/2*Pi -> 0.0, -Pi -> -1.0, -1.0/2*Pi -> 0.0, 0.0 -> 1.0, 1.0/2*Pi -> 0.0, Pi -> -1.0, 3.0/2*Pi -> 0.0, 2*Pi -> 1.0)
   
   override def sin(x: Exp[Double]) = x match {
    case Const(f) => if (sin_values.contains(f)) Const(sin_values(f).asInstanceOf[Double]) else unit(math.sin(f))
    case _ => super.sin(x)
  }
  
  override def cos(x: Exp[Double]) =  x match {
    case Const(f) => if (cos_values.contains(f)) Const(cos_values(f).asInstanceOf[Double]) else unit(math.cos(f))
    case _ => super.cos(x)
  }
  
}

trait ArithExpOpsPower extends PrimitiveOpsExp {
  
  override def double_times(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext)= (x, y) match {
    case (x, Const(0.0)) => Const(0.0)
    case (Const(0.0), y) => Const(0.0)
  	case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(-1.0)) => double_minus(Const(0.0), x)
    case (Const(-1.0), y) => double_minus(Const(0.0), y)
    case _ => super.double_times(x, y)
  }
}

trait ArithExpOpsFFT extends PrimitiveOpsExp {
  override def double_plus(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext)= (x, y) match {
    case (x, Const(0.0)) => x
    case (Const(0.0), y) => y
    case _ => super.double_plus(x, y)
  }
  
  override def double_minus(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext)= (x, y) match {
    case (x, Const(0.0)) => x
    case _ => super.double_minus(x, y)
  }
  
  override def double_times(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext)= (x, y) match {
    case (x, Const(0.0)) => Const(0.0)
    case (Const(0.0), y) => Const(0.0)
  	case (x, Const(1.0)) => x
    case (Const(1.0), y) => y
    case (x, Const(-1.0)) => double_minus(Const(0.0), x)
    case (Const(-1.0), y) => double_minus(Const(0.0), y)
    case _ => super.double_times(x, y)
  }
}

trait ScalaGenTrig extends ScalaGenBase {
  val IR: TrigExpOpt
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case Cos(a) => emitValDef(sym, src"math.cos($a)")
		case Sin(a) => emitValDef(sym, src"math.sin($a)")
		case _ => super.emitNode(sym, rhs)
  	}
 }
