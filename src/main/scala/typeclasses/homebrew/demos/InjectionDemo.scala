package typeclasses.homebrew.demos

import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

sealed trait Inject
case class CaseA(n: Int) extends Inject
case class CaseB(s: String) extends Inject

object Inject {
  implicit def showA = Show[CaseA] {
    case CaseA(n) => s"-#!? Injected! $n ?!#-"
  }
}

object InjectionDemo {

  def main(args: Array[String]) {
    import Show._
    import shapeless._
    import ShowInstances._
    import typeclasses.generics.lazyImplicits._
    import Inject._
    //successfully uses injected instances, but only after import.
    //this could be made more elegant by refactoring some of the
    //typeclass code into a hierarchy with a final instantiation step
    //where custom instances could be injected.
    implicit val showInject = implicitly[Lazy[Show[Inject]]].value
    println((CaseA(12): Inject).show)
    println(CaseA(19).show)
  }
}
