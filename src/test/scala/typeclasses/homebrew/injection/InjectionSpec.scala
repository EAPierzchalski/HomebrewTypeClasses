package typeclasses.homebrew.injection

import org.specs2.Specification
import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/21/14.
 */

sealed trait Inject
case class CaseA(n: Int) extends Inject
case class CaseB(s: String) extends Inject

object Inject {
  implicit def showA = Show[CaseA] {
    case CaseA(n) => s"INJECTION($n)"
  }
}

sealed trait CRA
case class BA(s: String) extends CRA
case class RA(n: Int, b: CRB) extends CRA

sealed trait CRB
case class BB(n: Int) extends CRB
case class RB(d: Double, c: CRC) extends CRB

sealed trait CRC
case class BC(s: String) extends CRC
case class RC(s: String, a: CRA) extends CRC

class InjectionSpec extends Specification {
  def is = s2"""
  This is a spec for the injection of user-specified
  implicit instances into the auto-generated typeclasses.

  The auto Show typeclass for a simple, single-level trait tree should
  use a locally imported instance
    when showing a trait $e1
    when showing a case class $e2

  For a mutually recursive set of three traits, it should use a locally
  defined instance
    when showing a trait $e3
"""

  def e1 = {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    import Inject._
    val traitShow = (CaseA(12): Inject).show
    traitShow shouldEqual "CaseA {INJECTION(12)}"
  }

  def e2 = {
    import Show._
    import shapeless._
    import ShowInstances.auto._
    import Inject._
    val caseShow = CaseA(12).show
    caseShow shouldEqual "INJECTION(12)"
  }

  def e3 = {
    import Show._
    import shapeless._
    import ShowInstances.auto._

    implicit def injectedShow(implicit showCRC: Show[CRC]): Show[RB] = Show {
      case RB(d, c) => s"INJECTION($d, ${c.show})"
    }

    val recShow = (RA(1, RB(3.14, BC("hi"))): CRA).show
    recShow shouldEqual "RA {n: 1, b: RB {INJECTION(3.14, BC {s: hi})}}"
  }
}
