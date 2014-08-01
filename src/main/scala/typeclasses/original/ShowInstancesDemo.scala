package typeclasses.original

import typeclasses.homebrew.Show

/**
 * Created by eap on 8/1/14.
 */

sealed trait Rec
case class RNil(n: Int) extends Rec
case class RCons(s: String, rest: Rec) extends Rec

object Rec {
  implicit def injectedShowRNil = Show[RNil] {
    case RNil(n) => s"-!INJECTED $n!-"
  }
}

object ShowInstancesDemo {

  def main(args: Array[String]) {
    import Show._
    import ShowInstances._
    import ShowInstances.auto._
    import Rec._
    //sadly, no injection still.
    println((RCons("hi", RCons("lo", RNil(12))): Rec).show)
  }
}
