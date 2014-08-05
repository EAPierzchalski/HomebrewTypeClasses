package typeclasses.homebrew

import shapeless._
import shapeless.record.FieldType

/**
 * Created by eap on 7/31/14.
 */

trait TypeClassImpl[C[_]] {
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

}

trait TypeClass[C[_]] extends TypeClassImpl[C] { tc =>
  trait ProductRipper[L] extends DepFn0 {
    type Inner
    final type Out = C[Inner]
  }

  type ProductAux[L <: HList, I <: HList] = ProductRipper[L] {
    type Inner = I
  }

  type CoproductAux[L <: Coproduct, I <: Coproduct] = ProductRipper[L] {
    type Inner = I
  }

  type GenericAux[A, B] = ProductRipper[A] {
    type Inner = B
  }

  implicit def emptyProductRipper[In <: HNil]: ProductAux[In, HNil] =
    new ProductRipper[In] {
      type Inner = HNil
      def apply() = emptyProduct
    }

  implicit def productRipper[Label <: Symbol, Head, Tail <: HList, TailInner <: HList](
    implicit witness: Lazy[Witness.Aux[Label]],
    cHead: Lazy[C[Head]],
    tailRipper: Lazy[ProductAux[Tail, TailInner]]): ProductAux[FieldType[Label, Head] :: Tail, Head :: TailInner] =
    new ProductRipper[FieldType[Label, Head]:: Tail] {
      type Inner = Head :: TailInner
      def apply() = product(witness.value.value.name, cHead.value, tailRipper.value())
    }

  implicit def emptyCoproductRipper[In <: CNil]: CoproductAux[In, CNil] =
    new ProductRipper[In] {
      type Inner = CNil
      def apply() = emptyCoproduct
    }

  implicit def coproductRipper[Label <: Symbol, Left, Right <: Coproduct, RightInner <: Coproduct](
    implicit witness: Lazy[Witness.Aux[Label]],
    cLeft: Lazy[C[Left]],
    rightRipper: Lazy[CoproductAux[Right, RightInner]]): CoproductAux[FieldType[Label, Left] :+: Right, Left :+: RightInner] =
    new ProductRipper[FieldType[Label, Left]:+: Right] {
      type Inner = Left :+: RightInner
      def apply() = coproduct(witness.value.value.name, cLeft.value, rightRipper.value())
    }

  implicit def genericRipper[A, Repr0, Repr1](
    implicit lg: Lazy[LabelledGeneric.Aux[A, Repr0]],
    bg: Lazy[Generic.Aux[A, Repr1]],
    ripper: Lazy[GenericAux[Repr0, Repr1]]): C[A] =
    project(ripper.value(), bg.value.to, bg.value.from)

}
