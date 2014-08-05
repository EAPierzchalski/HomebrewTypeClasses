package typeclasses.homebrew.demos

import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

object CoproductDemo {
  sealed trait Trait
  case class A(n: Int) extends Trait
  case class B(s: String, t: Trait) extends Trait
  sealed trait TT extends Trait
  case class C(d: Double) extends TT

  def main(args: Array[String]) {
    import typeclasses.generics.lazyImplicits._
    import shapeless._
    import Show._
    import ShowInstances._
    //reaches within tuples to find show instances

    //val lga = LabelledGeneric[C]
    //val bga = Generic[C]
    //ShowInstances.genericProductRipper[C, lga.Repr, bga.Repr]
    implicit val magic = implicitly[Lazy[Show[Trait]]].value
    println((B("magi", B("12", C(2.2))): Trait).show)
    //println(((B("magic", 12), A(14)): (Trait, Trait)).show)
  }
}
