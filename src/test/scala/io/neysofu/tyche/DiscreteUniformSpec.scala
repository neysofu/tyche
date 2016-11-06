package io.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class DiscreteUniformSpec extends WordSpec with Matchers {
 
  val sampleSize = 10000

  // Coin toss
  sealed trait Coin
  object Head extends Coin
  object Tail extends Coin
  val coin = DiscreteDistribution.uniform(Head, Tail)

  // 1-sided die
  object Outcome
  val die = DiscreteDistribution.uniform(Outcome)

  // Families
  sealed trait Child
  object Male extends Child
  object Female extends Child
  val family = DiscreteDistribution.uniform(Male, Female)
    .until(_ contains Male)
    .map(_.size - 1)

  "A discrete uniform probability distribution" when {
    
    "a coin is tossed" should {
      "return $HEAD or $TAIL." in {
        coin.get shouldBe a [Coin]
      }
      "take ~6 tosses to generate HH" in {
        val coinSpree = coin
          .until(_ containsSlice Seq(Head, Head))
          .map(_.size)
          .take(sampleSize)
          .sum
        Math.round(1.0 * coinSpree / sampleSize) shouldBe 6
      }
    }

    "a 1-sided die is rolled" should {
      "always return the same value" in {
        die.get shouldBe Outcome
      }
    }

    "parents only want boys" should {
      "compute an expectation of one girl per family" in {
        Math.round(
          1.0 * family.take(sampleSize).sum / sampleSize
        ) shouldBe 1
      }
    }
  }
}
