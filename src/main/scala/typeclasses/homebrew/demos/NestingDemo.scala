package typeclasses.homebrew.demos

import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

sealed trait CorecA
case class BaseA(s: String) extends CorecA
case class RecA(n: Int, b: CorecB) extends CorecA

sealed trait CorecB
case class BaseB(n: Int) extends CorecB
case class RecB(d: Double, c: CorecC) extends CorecB

sealed trait CorecC
case class BaseC(d: Double) extends CorecC
case class RecC(s: String, a: CorecA) extends CorecC

sealed trait Inside
case class In(s: String) extends Inside

sealed trait Outside
case class Out(i: Inside) extends Outside

case class BareIn(s: String)
case class BareOut(bi: BareIn)

object NestingDemo {
  def main(args: Array[String]) {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    println((RecA(1, RecB(3.14, BaseC(12.0))): CorecA).show)
    println((Out(In("sup")): Outside).show)
    println(BareOut(BareIn("lol")).show)
  }
}
