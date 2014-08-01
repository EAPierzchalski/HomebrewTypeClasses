package typeclasses.homebrew.demos

import typeclasses.homebrew.{ Show, ShowInstances }

/**
 * Created by eap on 7/31/14.
 */

sealed trait Rec
case class RNil(n: Int) extends Rec
case class RCons(s: String, rest: Rec) extends Rec

object RecursionDemo {
  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //fails to handle recursive types: 'diverging implicit expansion'.
    //presumably using Lazy[_] will help, but whatever I tried wasn't very
    //successful.
    //println((RCons("hi", RCons("lo", RNil(12))): Rec).show)
  }
}

