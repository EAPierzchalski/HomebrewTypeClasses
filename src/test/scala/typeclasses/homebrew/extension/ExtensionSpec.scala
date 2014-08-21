package typeclasses.homebrew.extension

import org.specs2.Specification
import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/21/14.
 */

sealed trait Trait
case class A(n: Int) extends Trait
case class B(s: String, t: Trait) extends Trait
sealed trait TT extends Trait
case class C(d: Double) extends TT

class ExtensionSpec extends Specification {
  def is = s2"""
  This is a spec for the use of auto-generated typeclasses
  inside of other, implicitly-defined typeclasses.

  The auto Show typeclass should
    work for plain old instances $e1
    interpolate into '(A, A)' $e2
    interpolate into 'Seq[A]' $e3
"""

  def e1 = {
    import shapeless._
    import Show._
    import ShowInstances.auto._
    val showRes = (B("magi", B("12", C(2.2))): Trait).show
    showRes mustEqual "B {s: magi, t: B {s: 12, t: C {d: 2.2}}}"
  }

  def e2 = {
    import shapeless._
    import Show._
    import ShowInstances.auto._
    val showRes = ((B("magic", C(3.15)), A(14)): (Trait, Trait)).show
    showRes mustEqual "(B {s: magic, t: C {d: 3.15}}, A {n: 14})"
  }

  def e3 = {
    import shapeless._
    import Show._
    import ShowInstances.auto._
    val showRes = Seq[Trait](A(12), C(3.14)).show
    showRes mustEqual "Seq(A {n: 12}, C {d: 3.14})"
  }
}
