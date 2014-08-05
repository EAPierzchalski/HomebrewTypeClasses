package typeclasses.generics

import shapeless.{ Generic, Lazy, LabelledGeneric }

/**
 * Created by eap on 8/5/14.
 */
object lazyImplicits {
  implicit def lazyLabGen[A](
    implicit gen: LabelledGeneric[A]): Lazy[LabelledGeneric.Aux[A, gen.Repr]] = Lazy { gen }
  implicit def lazyGen[A](
    implicit gen: Generic[A]): Lazy[Generic.Aux[A, gen.Repr]] = Lazy { gen }
}
