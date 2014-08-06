package typeclasses.homebrew.showDemos

import typeclasses.homebrew.show.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

//for some reason these need to be outside of an object.
sealed trait CorecA
case class BaseA(s: String) extends CorecA
case class RecA(n: Int, b: CorecB) extends CorecA

sealed trait CorecB
case class BaseB(n: Int) extends CorecB
case class RecB(d: Double, c: CorecC) extends CorecB

sealed trait CorecC
case class BaseC(s: String) extends CorecC
case class RecC(s: String, a: CRA) extends CorecC

object NestingDemo {
  sealed trait Inside
  case class In(s: String) extends Inside

  sealed trait Outside
  case class Out(i: Inside) extends Outside

  case class BareIn(s: String)
  case class BareOut(bi: BareIn)

  def main(args: Array[String]) {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    println((RA(1, RB(3.14, BC("hi"))): CRA).show)
    println((Out(In("sup")): Outside).show)
    println(BareOut(BareIn("lol")).show)
  }
}
