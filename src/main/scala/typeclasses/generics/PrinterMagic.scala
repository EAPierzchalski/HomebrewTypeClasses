package typeclasses.generics

import shapeless._

/**
 * Created by eap on 8/4/14.
 */
object PrinterMagic {
  trait Printer[A] extends DepFn0 {
    type Out = A => String
  }

  implicit def emptyProduct[In <: HNil]: Printer[In] =
    new Printer[In] {
      def apply() = _ => "HNil"
    }

  implicit def product[Head, Tail <: HList](
    implicit tailPrinter: Printer[Tail]): Printer[Head :: Tail] =
    new Printer[Head :: Tail] {
      def apply() = {
        case h :: t => s"${h.toString} :*: ${tailPrinter()(t)}"
      }
    }

  implicit def genericProduct[A, Repr <: HList](
    implicit gen: Generic.Aux[A, Repr],
    reprPrinter: Printer[Repr]): Printer[A] =
    new Printer[A] {
      override def apply() = a => reprPrinter()(gen.to(a))
    }

  implicit def emptyCoproduct[In <: CNil]: Printer[In] =
    new Printer[In] {
      def apply() = throw new Exception("should never try and print CNil")
    }

  implicit def coproduct[Left, Right <: Coproduct](
    implicit rightPrinter: Printer[Right]): Printer[Left :+: Right] =
    new Printer[Left :+: Right] {
      def apply() = {
        case Inl(left) => left.toString
        case Inr(right) => rightPrinter()(right)
      }
    }

  implicit def genericCoproduct[A, Repr <: Coproduct](
    implicit gen: Generic.Aux[A, Repr],
    reprPrinter: Printer[Repr]): Printer[A] =
    new Printer[A] {
      def apply() = a => reprPrinter()(gen.to(a))
    }

  def apply[A](implicit printer: Printer[A]): A => String = printer()
}

object PrinterMagicDemo {
  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String, t: T) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

  def main(args: Array[String]) {
    val bPrinter = PrinterMagic[B]
    val tPrinter = PrinterMagic[T]
    println(bPrinter(B("hi", A(12))))
    println(tPrinter(A(12)))
  }
}