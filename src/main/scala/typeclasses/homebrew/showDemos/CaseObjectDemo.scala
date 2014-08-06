package typeclasses.homebrew.showDemos

import typeclasses.homebrew.show.{ Show, ShowInstances }

/**
 * Created by eap on 8/6/14.
 */
object CaseObjectDemo {
  sealed trait IntList
  case class Cons(n: Int, l: IntList) extends IntList
  case object Nil extends IntList

  def main(args: Array[String]) {
    import shapeless._
    import Show._
    import ShowInstances.auto._

    //having to do this is unfortunate.
    //if we define `case class Nil()`, we avoid having
    //to do this explicitly.
    implicit def showNil = Show[Nil.type] {
      case _ => ""
    }
    println((Cons(5, Cons(6, Nil)): IntList).show)
  }
}
