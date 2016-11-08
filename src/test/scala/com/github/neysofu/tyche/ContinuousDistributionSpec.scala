package com.github.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class ContinuousDistributionSpec extends WordSpec with Matchers {

  "A normal distribution" when {
    
    "μ = ∞" should {
      "ignore σ²" in {
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
      val gen = ContinuousDistribution.chiSquare(1)
      "yield a nonnegative value" in {
        gen.get should be >= 0.0
      }
      "have a nonnegative mean" in {
        gen.mean should be >= 0.0
      }
    }
  }

  "A binomial distribution" when {

    val n = 3

    "P = 1" should {
      "yield n" in {
        ContinuousDistribution.binomial(n, 1).get shouldBe n
      }
    }

    "P = 0" should {
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

  "A pseudo-continuous distribution" when {
    
    val gen = new ContinuousDistribution(() => 1.0)
    
    "its sample space size equals 1" should {
      "have equal mean and outcome" in {
        (gen.get - gen.mean).abs should be < 0.0001
      }
      "have a null standard deviation" in {
        gen.standardDeviation.abs should be < 0.0001
      }
    }
  }
}
