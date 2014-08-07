package typeclasses.homebrew.demos

import typeclasses.homebrew.{ Show, ShowInstances }

/**
 * Created by eap on 8/7/14.
 */

sealed trait InnerRec
case class Base(s: String) extends InnerRec
case class ListTree(s: String, children: List[InnerRec]) extends InnerRec

object InnerRecursionDemo {
  def main(args: Array[String]) {
    import Show._
    import shapeless._
    import ShowInstances.auto._

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

    import shapeless.test.illTyped
    illTyped { """println(listTree.show)""" }
  }
}
