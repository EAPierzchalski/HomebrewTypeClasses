package typeclasses.original

import shapeless._
import typeclasses.homebrew.Show

/**
 * Created by eap on 8/1/14.
 */
object ShowInstances extends LabelledTypeClassCompanion[Show] {
  import Show._
  implicit def instances: LabelledTypeClass[Show] = new LabelledTypeClass[Show] {
    override def emptyProduct: Show[HNil] = Show { _ => "" }

    override def project[F, G](
      instance: => Show[G],
      to: (F) => G,
      from: (G) => F): Show[F] =
      Show { a => instance.show(to(a)) }

    override def product[H, T <: HList](
      name: String,
      CHead: Show[H],
      CTail: Show[T]): Show[::[H, T]] =
      Show {
        case head :: tail =>
          val hs = CHead.show(head)
          val ts = CTail.show(tail)
          val end = if (ts.isEmpty) "" else s", $ts"
          s"$name = $hs$end"
      }

    override def emptyCoproduct: Show[CNil] =
      Show { _ => throw new Exception("Should never call instance for CNil") }

    override def coproduct[L, R <: Coproduct](
      name: String,
      CL: => Show[L],
      CR: => Show[R]): Show[:+:[L, R]] = Show {
      case Inl(left) => s"$name (${CL.show(left)})"
      case Inr(right) => CR.show(right)
    }
  }
}
