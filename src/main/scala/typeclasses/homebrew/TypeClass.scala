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
  trait ProductRipper[L <: HList] extends DepFn0 {
    type Inner <: HList
    type Out = C[Inner]
  }

  type ProductAux[L <: HList, I <: HList] = ProductRipper[L] {
    type Inner = I
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

  implicit def genericProductRipper[A, Repr0 <: HList, Repr1 <: HList](
    implicit lg: Lazy[LabelledGeneric.Aux[A, Repr0]],
    bg: Lazy[Generic.Aux[A, Repr1]],
    ripper: Lazy[ProductAux[Repr0, Repr1]]): C[A] =
    project(ripper.value(), bg.value.to, bg.value.from)

}
