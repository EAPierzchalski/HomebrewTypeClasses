package typeclasses.generics

import shapeless.record.FieldType
import shapeless._

/**
 * Created by eap on 8/4/14.
 */

object GenericImplicits {
  implicit def lazyLabGen[A](
    implicit gen: LabelledGeneric[A]): Lazy[LabelledGeneric.Aux[A, gen.Repr]] = Lazy { gen }
  implicit def lazyGen[A](
    implicit gen: Generic[A]): Lazy[Generic.Aux[A, gen.Repr]] = Lazy { gen }
}

object SymbolRipper {
  trait Ripper[A] extends DepFn0 {
    type Out <: HList
  }

  implicit def emptyProduct[In <: HNil]: Ripper[In] =
    new Ripper[In] {
      type Out = HNil
      def apply() = HNil
    }

  implicit def product[Label <: Symbol, Head, Tail <: HList](
    implicit labelWitness: Lazy[Witness.Aux[Label]],
    tailRipper: Lazy[Ripper[Tail]]): Ripper[FieldType[Label, Head] :: Tail] =
    new Ripper[FieldType[Label, Head]:: Tail] {
      type Out = String :: tailRipper.value.Out
      def apply() = labelWitness.value.value.name :: tailRipper.value()
    }

  implicit def genericProduct[A, Repr <: HList](
    implicit generic: Lazy[LabelledGeneric.Aux[A, Repr]],
    reprRipper: Lazy[Ripper[Repr]]): Ripper[A] =
    new Ripper[A] {
      type Out = reprRipper.value.Out
      def apply() = reprRipper.value()
    }

  implicit def emptyCoproduct[In <: CNil]: Ripper[In] =
    new Ripper[In] {
      type Out = HNil
      def apply() = HNil
    }

  implicit def coproduct[Label <: Symbol, Left, Right <: Coproduct](
    implicit labelWitness: Lazy[Witness.Aux[Label]],
    tailRipper: Lazy[Ripper[Right]]): Ripper[FieldType[Label, Left] :+: Right] =
    new Ripper[FieldType[Label, Left]:+: Right] {
      type Out = String :: tailRipper.value.Out
      def apply() = labelWitness.value.value.name :: tailRipper.value()
    }

  implicit def genericCoproduct[A, Repr <: Coproduct](
    implicit generic: Lazy[LabelledGeneric.Aux[A, Repr]],
    reprRipper: Lazy[Ripper[Repr]]): Ripper[A] =
    new Ripper[A] {
      type Out = reprRipper.value.Out
      def apply() = reprRipper.value()
    }

  object auto {
    implicit def derive[A](implicit ripper: Ripper[A]): ripper.Out = ripper()
  }

  def apply[A](implicit ripper: Ripper[A]): ripper.Out = ripper()
}

object RipperDemo {
  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String, t: T) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

  def main(args: Array[String]) {
    import GenericImplicits._
    import SymbolRipper._

    def magic[A](implicit ripper: Ripper[A]): Unit = println(ripper())
    magic[B]
  }
}

/*
  import shapeless._
  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String, t: T) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

 */ 