package typeclasses.generics

import shapeless.DepFn0

/**
 * Created by eap on 8/4/14.
 */
object PrinterMagic {
  trait PrinterInstance[A] extends DepFn0 {
    type Out = A => String
  }
}
