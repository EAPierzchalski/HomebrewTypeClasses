package typeclasses.homebrew

import shapeless._
import shapeless.record.FieldType
import scala.language.higherKinds

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

trait LowPriorityTypeClassConstructors[C[_]] extends TypeClassImpl[C] {
  trait Instance[L] extends DepFn0 {
    type Inner
    final type Out = Lazy[C[Inner]]
  }

  type ProductAux[L <: HList, I <: HList] = Instance[L] {
    type Inner = I
  }

  type CoproductAux[L <: Coproduct, I <: Coproduct] = Instance[L] {
    type Inner = I
  }

  type GenericAux[A, B] = Instance[A] {
    type Inner = B
  }

  trait Wrapper[F[_], A, FA] {
    def apply(c: C[A]): C[FA]
  }

  implicit def idWrapper[A]: Wrapper[Id, A, A] = new Wrapper[Id, A, A] {
    def apply(c: C[A]) = c
  }

  implicit def emptyProductInstance[In <: HNil]: ProductAux[In, HNil] =
    new Instance[In] {
      type Inner = HNil
      def apply() = emptyProduct
    }

  implicit def productInstance[Label <: Symbol, F[_], Head, FHead, Tail <: HList, TailInner <: HList](
    implicit witness: Witness.Aux[Label],
    wrapper: Wrapper[F, Head, FHead],
    ev: F[Head] =:= FHead,
    cHead: Lazy[C[Head]],
    tailInstance: ProductAux[Tail, TailInner]): ProductAux[FieldType[Label, FHead] :: Tail, FHead :: TailInner] =
    new Instance[FieldType[Label, FHead]:: Tail] {
      type Inner = FHead :: TailInner
      def apply() = product(witness.value.name, wrapper(cHead.value), tailInstance().value)
    }

  implicit def emptyCoproductInstance[In <: CNil]: CoproductAux[In, CNil] =
    new Instance[In] {
      type Inner = CNil
      def apply() = emptyCoproduct
    }

  implicit def coproductInstance[Label <: Symbol, Left, Right <: Coproduct, RightInner <: Coproduct](
    implicit witness: Witness.Aux[Label],
    cLeft: Lazy[C[Left]],
    rightInstance: CoproductAux[Right, RightInner]): CoproductAux[FieldType[Label, Left] :+: Right, Left :+: RightInner] =
    new Instance[FieldType[Label, Left]:+: Right] {
      type Inner = Left :+: RightInner
      def apply() = coproduct(witness.value.name, cLeft.value, rightInstance().value)
    }

  implicit def genericInstance[A, Repr0, Repr1](
    implicit lg: LabelledGeneric.Aux[A, Repr0],
    bg: Generic.Aux[A, Repr1],
    instance: GenericAux[Repr0, Repr1]): FinalInstance[A] =
    new FinalInstance[A] {
      def apply() = project(instance().value, bg.to, bg.from)
    }

  trait FinalInstance[A] {
    def apply(): C[A]
  }
}

trait TypeClass[C[_]] extends LowPriorityTypeClassConstructors[C] {
  object auto {
    implicit def derive[A](
      implicit instance: Lazy[FinalInstance[A]]): C[A] =
      instance.value()
  }
}
