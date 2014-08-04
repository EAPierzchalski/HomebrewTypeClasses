package typeclasses.homebrew

import shapeless._
import shapeless.record._

/**
 * Created by eap on 7/31/14.
 */

trait TypeClass[C[_]] {
  def emptyProduct: C[HNil]
  def product[H, T <: HList](
    name: String,
    CHead: C[H],
    CTail: C[T]): C[H :: T]
  def emptyCoproduct: C[CNil]
  def coproduct[L, R <: Coproduct](
    name: String,
    CL: => C[L],
    CR: => C[R]): C[L :+: R]
  def project[A, B](
    instance: C[B],
    to: A => B,
    from: B => A): C[A]

  trait InstanceFor[A] extends DepFn0 {
    type Out <: C[_]
  }

  type Aux[A, Out0] = InstanceFor[A] {
    type Out = C[Out0]
  }

  implicit def emptyProductCase[In <: HNil]: Aux[In, HNil] =
    new InstanceFor[In] {
      type Out = C[HNil]
      override def apply(): Out = emptyProduct
    }

  implicit def productCase[Label <: Symbol, Head, Tail <: HList, TailOut <: HList](
    implicit witnessLabel: Lazy[Witness.Aux[Label]],
    headInstance: Lazy[C[Head]],
    tailInstance: Lazy[Aux[Tail, TailOut]]): Aux[FieldType[Label, Head] :: Tail, Head :: TailOut] =
    new InstanceFor[FieldType[Label, Head]:: Tail] {
      type Out = C[Head :: TailOut]
      override def apply(): Out =
        product(
          name = witnessLabel.value.value.name,
          CHead = headInstance.value,
          CTail = tailInstance.value()
        )
    }

  implicit def genericProductCase[A, LabelledRep <: HList, StrippedRep <: HList](
    implicit labelledGenA: Lazy[LabelledGeneric.Aux[A, LabelledRep]],
    bareGenA: Lazy[Generic.Aux[A, StrippedRep]],
    labelledInstance: Lazy[Aux[LabelledRep, StrippedRep]]): Aux[A, A] =
    new InstanceFor[A] {
      type Out = C[A]
      override def apply(): Out = project[A, StrippedRep](
        instance = labelledInstance.value(),
        to = bareGenA.value.to,
        from = bareGenA.value.from
      )
    }

  implicit def emptyCoproductCase[In <: CNil]: Aux[In, CNil] =
    new InstanceFor[In] {
      type Out = C[CNil]
      override def apply(): Out = emptyCoproduct
    }

  implicit def coproductCase[Label <: Symbol, Left, Right <: Coproduct, RightOut <: Coproduct](
    implicit witnessLabel: Lazy[Witness.Aux[Label]],
    headInstance: Lazy[C[Left]],
    tailInstance: Lazy[Aux[Right, RightOut]]): Aux[FieldType[Label, Left] :+: Right, Left :+: RightOut] =
    new InstanceFor[FieldType[Label, Left]:+: Right] {
      type Out = C[Left :+: RightOut]
      override def apply(): Out =
        coproduct(
          name = witnessLabel.value.value.name,
          CL = headInstance.value,
          CR = tailInstance.value()
        )
    }

  implicit def genericCoproductCase[A, LabelledRep <: Coproduct, StrippedRep <: Coproduct](
    implicit labelledGenA: Lazy[LabelledGeneric.Aux[A, LabelledRep]],
    bareGenA: Lazy[Generic.Aux[A, StrippedRep]],
    labelledInstance: Lazy[Aux[LabelledRep, StrippedRep]]): Aux[A, A] =
    new InstanceFor[A] {
      type Out = C[A]
      override def apply(): Out = project[A, StrippedRep](
        instance = labelledInstance.value(),
        to = bareGenA.value.to,
        from = bareGenA.value.from
      )
    }

  object auto {
    implicit def derive[A](implicit instance: Aux[A, A]): C[A] = instance()
  }

  def apply[A](implicit instance: Aux[A, A]): C[A] = instance()
}
