package typeclasses.homebrew.nesting

import org.specs2.Specification
import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/21/14.
 */

sealed trait CorecA
case class BaseA(s: String) extends CorecA
case class RecA(n: Int, b: CorecB) extends CorecA

sealed trait CorecB
case class BaseB(n: Int) extends CorecB
case class RecB(d: Double, c: CorecC) extends CorecB

sealed trait CorecC
case class BaseC(s: String) extends CorecC
case class RecC(s: String, a: CorecA) extends CorecC

sealed trait Inside
case class In(s: String) extends Inside

sealed trait Outside
case class Out(i: Inside) extends Outside

case class BareIn(s: String)
case class BareOut(bi: BareIn)

class NestingSpec extends Specification {
  def is = s2"""
  This is a spec for auto codecs on nested and recursive
  traits.

  The auto Show codec should
    handle nested traits $e1
    handle nested case classes $e2
    handle deep recursive traits $e3
"""

  def e1 = {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    val nestShow = (Out(In("sup")): Outside).show
    nestShow shouldEqual "Out {i: In {s: sup}}"
  }

  def e2 = {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    val bareShow = BareOut(BareIn("lol")).show
    bareShow shouldEqual "bi: s: lol"
  }

  def e3 = {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    val recShow = (RecA(1, RecB(3.14, RecC("hi", BaseA("done")))): CorecA).show
    recShow shouldEqual "RecA {n: 1, b: RecB {d: 3.14, c: RecC {s: hi, a: BaseA {s: done}}}}"
  }
}
