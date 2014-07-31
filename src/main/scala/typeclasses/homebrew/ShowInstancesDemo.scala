package typeclasses.homebrew

import shapeless._

/**
 * Created by eap on 7/31/14.
 */

object CoproductDemo {
  sealed trait T
  case class A(n: Int) extends T
  case class B(wop: String, n: Int) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //reaches within tuples to find show instances
    println(((B("magic", 12), A(14)): (T, T)).show)
  }
}

object RecursionDemo {
  sealed trait Rec
  case class RNil(n: Int) extends Rec
  case class RCons(s: String, rest: Rec) extends Rec

  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //fails to handle recursive types: 'diverging implicit expansion'.
    //presumably using Lazy[_] will help, but whatever I tried wasn't very
    //successful.
    //println((RCons("hi", RCons("lo", RNil(12))): Rec).show)
  }
}

object InjectionDemo {
  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String) extends T

  object T {
    implicit def showA = Show[A] {
      case A(n) => s"-#!? Injected! $n ?!#-"
    }
  }

  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    import T._
    //successfully uses injected instances, but only after import.
    //this could be made more elegant by refactoring some of the
    //typeclass code into a hierarchy with a final instantiation step
    //where custom instances could be injected.
    println((A(12): T).show)
  }
}

object NestingDemo {
  sealed trait T
  case class A(n: Int) extends T

  sealed trait U
  case class B(s: String, t: T) extends U

  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //fails to handle nested instances: another divering instance issue.
    //println((B("hello", A(12)): U).show)
  }
}
