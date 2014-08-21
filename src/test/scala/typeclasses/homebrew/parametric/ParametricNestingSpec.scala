package typeclasses.homebrew.parametric

import org.specs2.Specification
import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/21/14.
 */

sealed trait Simple
case class S1(s: String) extends Simple
case class S2(n: Int, s: Simple) extends Simple

sealed trait Nested
case class NBase(s: String) extends Nested
case class NSeq(ss: Seq[Simple]) extends Nested

class ParametricNestingSpec extends Specification {
  def is = s2"""
  This is a spec for auto codecs over traits with
  parametric nested values, i.e. trait trees with members of the form

    A(ts: F[B])

  For some F, B.

  The auto Show typeclass should
    handle Seq nesting $e1
"""

  def e1 = {
    import shapeless._
    import Show._
    import ShowInstances.auto._

    implicit def lzss[A](implicit lzs: Lazy[Show[A]]): Lazy[Show[Seq[A]]] = Lazy {
      Show.showSeq(lzs.value)
    }

    import shapeless.test.illTyped
    illTyped {
      """
        val nestedShow = (NSeq(Seq(S1("hi"), S1("lo"), S2(12, S1("rec")))): Nested).show
      """
    }
    val nestedShow = ""
    nestedShow shouldEqual "NSeq {ss: Seq(S1 {s: hi}, S1 {s: lo}, S2 {n: 12, s: S1 {s: rec}}}})}"
  }
}
