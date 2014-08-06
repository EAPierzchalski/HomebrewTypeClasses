package typeclasses.homebrew.json

import argonaut._
import Argonaut._
import shapeless._
import typeclasses.homebrew.TypeClass

/**
 * Created by eap on 8/6/14.
 */

object EncodeJsonInstances extends TypeClass[EncodeJson] {
  def emptyProduct: EncodeJson[HNil] =
    EncodeJson {
      _ => jEmptyObject
    }

  def product[H, T <: HList](
    name: String,
    CHead: EncodeJson[H],
    CTail: EncodeJson[T]): EncodeJson[::[H, T]] =
    EncodeJson {
      case head :: tail => Json(name := head.jencode(CHead)) deepmerge tail.jencode(CTail)
    }

  def coproduct[L, R <: Coproduct](
    name: String,
    CL: => EncodeJson[L],
    CR: => EncodeJson[R]): EncodeJson[:+:[L, R]] =
    EncodeJson {
      case Inl(left) => Json(name := left.jencode(CL))
      case Inr(right) => right.jencode(CR)
    }

  def project[A, B](
    instance: EncodeJson[B],
    to: (A) => B,
    from: (B) => A): EncodeJson[A] =
    instance contramap to

  def emptyCoproduct: EncodeJson[CNil] =
    EncodeJson {
      _ => throw new scala.IllegalArgumentException("Tried to encode CNil")
    }
}

object DecodeJsonInstances extends TypeClass[DecodeJson] {
  def emptyProduct: DecodeJson[HNil] =
    DecodeJson {
      c =>
        c.focus match {
          case `jEmptyObject` => DecodeResult.ok(HNil)
          case _ => DecodeResult.fail("Tried to decode HNil from nonempty object", c.history)
        }
    }

  def product[H, T <: HList](
    name: String,
    CHead: DecodeJson[H],
    CTail: DecodeJson[T]): DecodeJson[::[H, T]] =
    DecodeJson {
      c =>
        val headCursor = c --\ name
        val tailCursor = headCursor.deleteGoParent
        for {
          head <- headCursor jdecode CHead
          tail <- tailCursor jdecode CTail
        } yield head :: tail
    }

  def coproduct[L, R <: Coproduct](
    name: String,
    CL: => DecodeJson[L],
    CR: => DecodeJson[R]): DecodeJson[:+:[L, R]] =
    DecodeJson {
      c =>
        def left = (c --\ name).jdecode(CL) map Inl.apply
        def right = c.jdecode(CR) map Inr.apply
        left ||| right
    }

  def project[A, B](
    instance: DecodeJson[B],
    to: (A) => B,
    from: (B) => A): DecodeJson[A] =
    instance map from

  def emptyCoproduct: DecodeJson[CNil] =
    DecodeJson {
      c => DecodeResult.fail("Tried to decode CNil", c.history)
    }
}