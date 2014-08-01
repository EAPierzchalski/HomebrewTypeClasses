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
    CH: C[H],
    CT: C[T]): C[H :: T]
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
    implicit witnessLabel: Lazy[Witness.Aux[Label]],
    headInstance: Lazy[C[Head]],
    tailInstance: Lazy[Aux[Tail, TailOut]]): Aux[FieldType[Label, Head] :: Tail, Head :: TailOut] =
    new InstanceFor[FieldType[Label, Head]:: Tail] {
      type Inner = Head :: TailOut
      override def apply(): Out =
        productInstance(
          name = witnessLabel.value.value.name,
          CH = headInstance.value,
          CT = tailInstance.value()
        )
    }

  implicit def cnilCase[In <: CNil]: Aux[In, CNil] =
    new InstanceFor[In] {
      type Inner = CNil
      override def apply(): Out = emptyCoproduct
    }

  implicit def coproductCase[Label <: Symbol, Left, Right <: Coproduct, TailOut <: Coproduct](
    implicit witnessLabel: Lazy[Witness.Aux[Label]],
    headInstance: Lazy[C[Left]],
    tailInstance: Lazy[Aux[Right, TailOut]]): Aux[FieldType[Label, Left] :+: Right, Left :+: TailOut] =
    new InstanceFor[FieldType[Label, Left]:+: Right] {
      type Inner = Left :+: TailOut
      override def apply(): Out =
        coproductInstance(
          name = witnessLabel.value.value.name,
          CL = headInstance.value,
          CR = tailInstance.value()
        )
    }

  implicit def genericCase[A, LabelledRep, StrippedRep](
    implicit labelledGenA: LabelledGeneric.Aux[A, LabelledRep],
    bareGenA: Generic.Aux[A, StrippedRep],
    labelledInstance: Lazy[Aux[LabelledRep, StrippedRep]]): Aux[A, A] =
    new InstanceFor[A] {
      type Inner = A
      override def apply(): Out = project[A, StrippedRep](
        instance = labelledInstance.value(),
        to = bareGenA.to,
        from = bareGenA.from
      )
    }

  object auto {
    implicit def derive[A](implicit instance: Aux[A, A]): C[A] = instance()
  }

  def apply[A](implicit instance: InstanceFor[A] { type Inner = A }) = instance()
}
