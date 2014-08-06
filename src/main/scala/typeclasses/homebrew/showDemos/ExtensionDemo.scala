package typeclasses.homebrew.showDemos

import typeclasses.homebrew.show.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

object ExtensionDemo {
  sealed trait Trait
  case class A(n: Int) extends Trait
  case class B(s: String, t: Trait) extends Trait
  sealed trait TT extends Trait
  case class C(d: Double) extends TT

  def main(args: Array[String]) {
    import shapeless._
    import Show._
    import ShowInstances.auto._
    println((B("magi", B("12", C(2.2))): Trait).show)
    println(((B("magic", C(3.15)), A(14)): (Trait, Trait)).show)
    println(Seq[Trait](A(12), C(3.14)).show)
  }
}
