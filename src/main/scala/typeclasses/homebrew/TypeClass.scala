package typeclasses.homebrew

import shapeless._
import shapeless.record._

/**
 * Created by eap on 7/31/14.
 */

trait TypeClass[C[_]] {
  def emptyProduct: C[HNil]
  def productInstance[H, T <: HList](
    name: String,
    CH: => C[H],
    CT: => C[T]): C[H :: T]
  def emptyCoproduct: C[CNil]
  def coproductInstance[L, R <: Coproduct](
    name: String,
    CL: => C[L],
    CR: => C[R]): C[L :+: R]
  def project[A, B](
    instance: C[B],
    to: A => B,
    from: B => A): C[A]

  trait InstanceFor[A] extends DepFn0 {
    type Inner
    final type Out = C[Inner]
  }

  type Aux[A, Inner0] = InstanceFor[A] {
    type Inner = Inner0
  }

  implicit def hnilCase[In <: HNil]: Aux[In, HNil] =
    new InstanceFor[In] {
      type Inner = HNil
      override def apply(): Out = emptyProduct
    }

  implicit def productCase[Label <: Symbol, Head, Tail <: HList, TailOut <: HList](
    implicit witnessLabel: Witness.Aux[Label],
    headInstance: C[Head],
    tailInstance: Aux[Tail, TailOut]): Aux[FieldType[Label, Head] :: Tail, Head :: TailOut] =
    new InstanceFor[FieldType[Label, Head]:: Tail] {
      type Inner = Head :: TailOut
      override def apply(): Out =
        productInstance(
          name = witnessLabel.value.name,
          CH = headInstance,
          CT = tailInstance()
        )
    }

  implicit def cnilCase[In <: CNil]: Aux[In, CNil] =
    new InstanceFor[In] {
      type Inner = CNil
      override def apply(): Out = emptyCoproduct
    }

  implicit def coproductCase[Label <: Symbol, Left, Right <: Coproduct, TailOut <: Coproduct](
    implicit witnessLabel: Witness.Aux[Label],
    headInstance: C[Left],
    tailInstance: Aux[Right, TailOut]): Aux[FieldType[Label, Left] :+: Right, Left :+: TailOut] =
    new InstanceFor[FieldType[Label, Left]:+: Right] {
      type Inner = Left :+: TailOut
      override def apply(): Out =
        coproductInstance(
          name = witnessLabel.value.name,
          CL = headInstance,
          CR = tailInstance()
        )
    }

  implicit def genericCase[A, LabelledRep, StrippedRep](
    implicit labelledGenA: LabelledGeneric[A] { type Repr = LabelledRep },
    bareGenA: Generic[A] { type Repr = StrippedRep },
    labelledInstance: InstanceFor[LabelledRep] { type Inner = StrippedRep }): InstanceFor[A] { type Inner = A } =
    new InstanceFor[A] {
      type Inner = A
      override def apply(): Out = project[A, StrippedRep](
        instance = labelledInstance(),
        to = bareGenA.to,
        from = bareGenA.from
      )
    }

  object auto {
    implicit def derive[A](implicit instance: InstanceFor[A] { type Inner = A }): C[A] = instance()
  }

  def apply[A](implicit instance: InstanceFor[A] { type Inner = A }) = instance()
}
