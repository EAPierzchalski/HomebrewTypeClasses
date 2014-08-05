package typeclasses.generics

import shapeless.record.FieldType
import shapeless._

/**
 * Created by eap on 8/4/14.
 */

object SymbolRipper {

  trait Ripper[A] extends DepFn0 {
    type Out <: HList
  }

  implicit def emptyProductRipper[In <: HNil]: Ripper[In] =
    new Ripper[In] {
      type Out = HNil
      def apply() = HNil
    }

  implicit def productRipper[Label <: Symbol, Head, Tail <: HList](
    implicit labelWitness: Lazy[Witness.Aux[Label]],
    tailRipper: Lazy[Ripper[Tail]]): Ripper[FieldType[Label, Head] :: Tail] =
    new Ripper[FieldType[Label, Head]:: Tail] {
      type Out = String :: tailRipper.value.Out
      def apply() = labelWitness.value.value.name :: tailRipper.value()
    }

  implicit def genericProductRipper[A, Repr <: HList, Repr2 <: HList](
    implicit generic: Lazy[LabelledGeneric.Aux[A, Repr]],
    otherGeneric: Lazy[Generic.Aux[A, Repr2]],
    ev: Repr <:< Repr2,
    reprRipper: Lazy[Ripper[Repr]]): Ripper[A] =
    new Ripper[A] {
      type Out = reprRipper.value.Out
      def apply() = reprRipper.value()
    }

  implicit def emptyCoproductRipper[In <: CNil]: Ripper[In] =
    new Ripper[In] {
      type Out = HNil
      def apply() = HNil
    }

  implicit def coproductRipper[Label <: Symbol, Left, Right <: Coproduct](
    implicit labelWitness: Lazy[Witness.Aux[Label]],
    tailRipper: Lazy[Ripper[Right]]): Ripper[FieldType[Label, Left] :+: Right] =
    new Ripper[FieldType[Label, Left]:+: Right] {
      type Out = String :: tailRipper.value.Out
      def apply() = labelWitness.value.value.name :: tailRipper.value()
    }

  implicit def genericCoproductRipper[A, Repr <: Coproduct](
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

/*
  import shapeless._
  sealed trait T
  case class A(n: Int) extends T
  case class B(s: String, t: T) extends T
  sealed trait TT extends T
  case class C(d: Double) extends TT

 */ 