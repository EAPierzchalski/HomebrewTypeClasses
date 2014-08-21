package typeclasses.homebrew.recursion

import org.specs2.Specification
import typeclasses.homebrew.{ Show, ShowInstances }

/**
 * Created by eap on 8/21/14.
 */

sealed trait Rec
case class RNil(n: Int) extends Rec
case class RCons(s: String, rest: Rec) extends Rec

class RecursionSpec extends Specification {
  def is = s2"""
  This is a spec for automatically generated typeclasses
  for simply recursive types. Here, we test a simple
  cons list:

    s t Rec
    c c RNil(n: Int) e Rec
    c c RCons(s: String, rest: Rec) e Rec

  The auto Show typeclass should
    handle simple recursive types $e1
"""
  def e1 = {
    import shapeless._
    import ShowInstances.auto._
    import Show._

    val recShow = (RCons("hi", RCons("lo", RNil(12))): Rec).show
    recShow shouldEqual "RCons {s: hi, rest: RCons {s: lo, rest: RNil {n: 12}}}"
  }
}
