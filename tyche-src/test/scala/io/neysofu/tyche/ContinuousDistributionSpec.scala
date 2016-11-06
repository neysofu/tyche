package io.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class ContinuousDistributionSpec extends WordSpec with Matchers {

  "A normal distribution" when {
    
    "μ = ∞" should {
      "ignore σ²" in {
        // Why on Earth would someone in their right minds define 1/0 as
        // infinite...
        ContinuousDistribution.normal(1, 1.0 / 0.0).get.isInfinity shouldBe true
      }
    }

    "σ² = 0" should {
      "yield μ" in {
        ContinuousDistribution.normal(0, 1).get shouldBe 1
      }
    }
  }

  "A chi-squared distribution" when {
    
    "k = 0" should {
      "yield 0" in {
        ContinuousDistribution.chiSquare(0).get shouldBe 0
      }
    }

    "k > 0" should {
      "yield a nonnegative value" in {
        ContinuousDistribution.chiSquare(1).get should be >= 0.0
      }
    }
  }

  "A binomial distribution" when {

    val n = 3

    "p = 1" should {
      "yield n" in {
        ContinuousDistribution.binomial(n, 1).get shouldBe n
      }
    }

    "p = 0" should {
      "yield 0" in {
        ContinuousDistribution.binomial(n, 0).get shouldBe 0
      }
    }

    "sampled" should {
      "yield a nonnegative, integer value" in {
        val outcome = ContinuousDistribution.binomial(n, Random.nextDouble).get
        outcome.isWhole shouldBe true
        outcome should be >= 0.0
      }
    }
  }

  "A continuous uniform distribution" when {
    
    "sampled" should {
      "yield a value that falls within the ´[0, 1[´ interval" in {
        val outcome = ContinuousDistribution.uniform.get
        outcome should be < 1.0
        outcome should be >= 0.0
      }
    }
  }
}
