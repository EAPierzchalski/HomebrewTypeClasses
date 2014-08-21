package typeclasses.scratch

import shapeless.Lazy
import typeclasses.homebrew.Show

/**
 * Created by eap on 8/21/14.
 */
object ShapelessLazy {
  sealed trait List[+T]
  case class Cons[T](hd: T, tl: List[T]) extends List[T]
  sealed trait Nil extends List[Nothing]
  case object Nil extends Nil

  def main(args: Array[String]) {
    // Base case for Int
    implicit def showInt: Show[Int] = new Show[Int] {
      def show(t: Int) = t.toString
    }

    // Base case for Nil
    implicit def showNil: Show[Nil] = new Show[Nil] {
      def show(t: Nil) = "Nil"
    }

    // Case for Cons[T]: note (mutually) recursive implicit argument referencing Show[List[T]]
    implicit def showCons[T](
      implicit st: Lazy[Lazy[Show[T]]],
      sl: Lazy[Show[List[T]]]): Show[ShapelessLazy.Cons[T]] =
      Show[ShapelessLazy.Cons[T]] {
        t => s"Cons(${st.value.value.show(t.hd)}, ${sl.value.show(t.tl)})"
      }

    // Case for List[T]: note (mutually) recursive implicit argument referencing Show[Cons[T]]
    implicit def showList[T](
      implicit sc: Lazy[Show[Cons[T]]],
      sn: Lazy[Show[Nil]]): Show[ShapelessLazy.List[T]] =

      Show[ShapelessLazy.List[T]] {
        case n: Nil => sn.value.show(n)
        case c: Cons[T] => sc.value.show(c)
      }

    trait Magic[A] {
      def shower: Show[A]
    }

    implicit def getMagic[A](implicit lzsa: Lazy[Show[A]]): Magic[A] = new Magic[A] {
      def shower = lzsa.value
    }

    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    def showLz[T](t: T)(implicit lzs: Lazy[Show[T]]) = lzs.value.show(t)
    def show[T](t: T)(implicit s: Show[T]) = s.show(t)

    implicitly[Lazy[Lazy[Show[List[Int]]]]]
    val f = implicitly[Lazy[Show[List[Int]]]].value.show _

    println(f(l))

    println(showLz(l))

    println(show(l))
  }
}
