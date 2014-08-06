package typeclasses.homebrew.demos

import typeclasses.homebrew.{ Show, ShowInstances }

/**
 * Created by eap on 7/31/14.
 */

object RecursionDemo {
  sealed trait Rec
  case class RNil(n: Int) extends Rec
  case class RCons(s: String, rest: Rec) extends Rec

  def main(args: Array[String]) {
    import shapeless._
    import ShowInstances.auto._
    import Show._
    println((RCons("hi", RCons("lo", RNil(12))): Rec).show)
  }
}

