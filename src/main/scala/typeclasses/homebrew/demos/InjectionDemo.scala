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
    //import ShowInstances.auto._
    import typeclasses.generics.lazyImplicits._
    import Inject._
    //successfully uses injected instances, but only after import.
    //this could be made more elegant by refactoring some of the
    //typeclass code into a hierarchy with a final instantiation step
    //where custom instances could be injected.
    //println((CaseA(12): Inject).show)
  }
}
