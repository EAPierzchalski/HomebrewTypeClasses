package typeclasses.scratch

import shapeless.Lazy

/**
 * Created by eap on 8/21/14.
 */

object ParameterRipping {

  sealed trait T
  case class S(ts: Seq[T]) extends T
  case class N(n: Int) extends T

  trait Dummy[A]

  def main(args: Array[String]) {
    implicit def dummyInt: Dummy[Int] = new Dummy[Int] {}
    implicit def dummyString: Dummy[String] = new Dummy[String] {}
    implicit def dummySeq[A](implicit dummyA: Lazy[Dummy[A]]): Dummy[Seq[A]] = {
      val x = dummyA.value
      new Dummy[Seq[A]] {}
    }

    def magic[A](a: A)(implicit d: Lazy[Dummy[A]]): A = a

    magic(5)
    magic("hi")
    magic(Seq(1, 2, 3))

    implicitly[Dummy[Seq[Seq[Seq[Int]]]]]

  }
}
