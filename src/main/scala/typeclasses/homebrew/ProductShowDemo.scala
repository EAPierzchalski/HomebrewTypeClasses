package typeclasses.homebrew

import shapeless._

/**
 * Created by eap on 7/31/14.
 */
object ProductShowDemo {
  sealed trait T
  case class A(n: Int) extends T
  case class B(wop: String, n: Int) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

  sealed trait Rec
  case class RNil(n: Int) extends Rec
  case class RCons(s: String, rest: Rec) extends Rec

  sealed trait Inj
  case class InjInt(n: Int) extends Inj
  case class InjString(s: String) extends Inj

  object InjInt {
    implicit def showInjInt = Show[InjInt] {
      case InjInt(n) => s"--injected! $n--"
    }
  }

  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //reaches within tuples to find show instances
    println(((B("magic", 12), A(14)): (T, T)).show)

    //fails to handle recursive types: 'diverging implicit expansion'.
    //presumably using Lazy[_] will help, but whatever I tried wasn't very
    //successful.
    //println((RCons("hi", RCons("lo", RNil(12))): Rec).show)

    //successfully uses injected instances, but only after import.
    //this could be made more elegant by refactoring some of the
    //typeclass code into a hierarchy with a final instantiation step
    //where custom instances could be injected.
    import InjInt._
    println((InjInt(5): Inj).show)
  }
}
