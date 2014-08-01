package typeclasses.homebrew.demos

import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

sealed trait Trait
case class A(n: Int) extends Trait
case class B(wop: String, n: Int) extends Trait
sealed trait TT extends Trait
case class C(d: Double) extends TT

object CoproductDemo {
  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //reaches within tuples to find show instances
    println(((B("magic", 12), A(14)): (Trait, Trait)).show)
  }
}
