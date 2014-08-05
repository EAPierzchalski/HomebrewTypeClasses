package typeclasses.homebrew.demos

import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

object CoproductDemo {
  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String, t: T) extends T
  sealed trait TT extends T
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
    implicitly[Show[C]]
    //println(B("magi", 12).show)
    //println(((B("magic", 12), A(14)): (Trait, Trait)).show)
  }
}
