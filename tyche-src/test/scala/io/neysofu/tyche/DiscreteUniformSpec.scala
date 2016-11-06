package io.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class DiscreteUniformSpec extends WordSpec with Matchers {
 
  val sampleSize = 10000
  
  // Families
  sealed trait Child
  object Male extends Child
  object Female extends Child
  val family = DiscreteDistribution.uniform(Male, Female)
    .until(_ contains Male)
    .map(_.size - 1)

  "A discrete uniform probability distribution" when {
    
      /*"take ~6 tosses to generate HH" in {
        val coinSpree = coin
          .until(_ containsSlice Seq(Head, Head))
          .map(_.size)
          .take(sampleSize)
          .sum
        Math.round(1.0 * coinSpree / sampleSize) shouldBe 6
      }*/

    "parents only want boys" should {
      "compute an expectation of one girl per family" in {
        Math.round(
          1.0 * family.take(sampleSize).sum / sampleSize
        ) shouldBe 1
      }
    }
  }
}
