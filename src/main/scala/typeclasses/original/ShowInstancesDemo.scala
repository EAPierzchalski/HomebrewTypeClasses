package typeclasses.original

import shapeless._

//Generic[_] and TypeClass[_] are known to have issues with
//sealed case class families inside of objects,
//so we pre-empt that by moving them to the top level.

sealed trait Rec
case class RNil(s: String) extends Rec
case class RCons(s: String, r: Rec) extends Rec

sealed trait InnerRec
case class Base(s: String) extends InnerRec
case class ListTree(s: String, children: List[InnerRec]) extends InnerRec

object ShowInstancesDemo {
  import ShowSyntax._

  /*
   There is nowhere we can stick this implicit such that
   it gets used by the shapeless LabelledTypeClass[_].
   */
  implicit def injectedShow: Show[RNil] = new Show[RNil] {
    override def show(t: RNil): String = s"Injected! ${t.s}"
  }

  def main(args: Array[String]) {
    import Show.auto._
    val out = (RCons("hi", RCons("lo", RNil("end"))): Rec).show
    //ideally, the `injectedShow` above would mean that
    //assert(out == "RCons(s = hi, r = RCons(s = lo, r = RNil(Injected! end)))

    //but instead,
    assert(out == "RCons(s = hi, r = RCons(s = lo, r = RNil(s = end)))")

    //ideally, lazy implicit resolution by the methods in Show.auto._
    //would mean that the compiler would have no issue deriving Show[List[InnerRec]]
    //and thus Show[InnerRec].
    val listTree: InnerRec =
      ListTree("top",
        List(
          Base("child"),
          ListTree("treeChild",
            List(
              Base("grandchild"),
              Base("otherGrandchild")
            )
          )
        )
      )

    //sadly...
    import shapeless.test.illTyped
    illTyped {
      """val out = listTree.show"""
    }
  }
}

object ShowSyntax {
  implicit class ShowSyntaxEnrich[A](a: A)(implicit shower: Show[A]) {
    def show: String = shower.show(a)
  }
}

trait Show[T] {
  def show(t: T): String
}

/**
 * The below is copied from the shapeless 'Show' example
 * (found [[https://github.com/milessabin/shapeless/blob/cc3af78992cc24fe64b25d27ab2fe878948be467/examples/src/main/scala/shapeless/examples/shows.scala here]])
 * I've added the 'listShow' to demonstrate problems when recursion is 'separated' by
 * an intermediate container type.
 */
object Show extends LabelledTypeClassCompanion[Show] {
  implicit def stringShow: Show[String] = new Show[String] {
    def show(t: String) = t
  }

  implicit def listShow[A](implicit showA: Show[A]): Show[List[A]] = new Show[List[A]] {
    def show(t: List[A]) = t.map(showA.show).mkString("List(", ", ", ")")
  }

  implicit def showInstance: LabelledTypeClass[Show] = new LabelledTypeClass[Show] {
    def emptyProduct = new Show[HNil] {
      def show(t: HNil) = ""
    }

    def product[F, T <: HList](name: String, CHead: Show[F], CTail: Show[T]) = new Show[F :: T] {
      def show(ft: F :: T) = {
        val head = CHead.show(ft.head)
        val tail = CTail.show(ft.tail)
        if (tail.isEmpty)
          s"$name = $head"
        else
          s"$name = $head, $tail"
      }
    }

    def emptyCoproduct = new Show[CNil] {
      def show(t: CNil) = ""
    }

    def coproduct[L, R <: Coproduct](name: String, CL: => Show[L], CR: => Show[R]) = new Show[L :+: R] {
      def show(lr: L :+: R) = lr match {
        case Inl(l) => s"$name(${CL.show(l)})"
        case Inr(r) => s"${CR.show(r)}"
      }
    }

    def project[F, G](instance: => Show[G], to: F => G, from: G => F) = new Show[F] {
      def show(f: F) = instance.show(to(f))
    }
  }
}
