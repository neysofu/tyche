package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class DiscreteUniformSpec extends WordSpec with Matchers {

  "A discrete uniform probability distribution" when {
    
    "a coin is tossed" should {

      sealed trait Coin
      case object Head extends Coin
      case object Tail extends Coin
      val gen = Commons.newDiscreteUniform(Seq(Head, Tail))

      "return $HEAD or $TAIL." in {
        gen.get shouldBe a [Coin]
      }
    }

    "a 1-sided die is rolled" should {
      
      case object Outcome
      val die = Commons.newDiscreteUniform(Seq(Outcome))

      "always return the same value" in {
        die.get shouldBe Outcome
      }
    }
  }
}
