package typeclasses.generics

import shapeless._

/**
 * Created by eap on 8/4/14.
 */

object RipperDemo {

  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String, t: T) extends T
  case class D(c: Char) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

  def main(args: Array[String]) {
    import lazyImplicits._
    import SymbolRipper._
    def magic[A](implicit ripp: Ripper[A]): Unit = println(ripp())
    magic[A]
    magic[D]
    magic[B]
    magic[C]
    magic[T]
  }
}

