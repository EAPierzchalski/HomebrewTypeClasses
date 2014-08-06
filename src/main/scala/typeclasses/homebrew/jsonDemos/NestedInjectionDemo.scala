package typeclasses.homebrew.jsonDemos

/**
 * Created by eap on 8/6/14.
 */

sealed trait CRA
case class BA(s: String) extends CRA
case class RA(n: Int, b: CRB) extends CRA

sealed trait CRB
case class BB(n: Int) extends CRB
case class RB(d: Double, c: CRC) extends CRB

sealed trait CRC
case class BC(s: String) extends CRC
case class RC(s: String, a: CRA) extends CRC

object NestedInjectionDemo {
  def main(args: Array[String]) {
    import shapeless._
    import argonaut._
    import Argonaut._
    import typeclasses.homebrew.json.EncodeJsonInstances.auto._

    implicit def injectRB(implicit encCRC: Lazy[EncodeJson[CRC]]): EncodeJson[RB] =
      EncodeJson {
        case RB(d, c) => Json("INJECTED" := d, "MORE_INJECTED" := c.jencode)
      }

    println((RA(1, RB(3.14, BC("hi"))): CRA).jencode.spaces2)
  }
}
