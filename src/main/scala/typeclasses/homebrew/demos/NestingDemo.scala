package typeclasses.homebrew.demos

import typeclasses.homebrew.{ ShowInstances, Show }

/**
 * Created by eap on 8/1/14.
 */

sealed trait Inner
case class Inside(n: Int) extends Inner

sealed trait Outer
case class Outside(s: String, t: Inner) extends Outer

object NestingDemo {
  def main(args: Array[String]) {
    import Show._
    import ShowInstances.auto._
    //fails to handle nested instances: another divering instance issue.
    //println((Outside("hello", Inside(12)): Outer).show)
  }
}
