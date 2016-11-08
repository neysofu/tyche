package com.github.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class DiscreteDistributionSpec extends WordSpec with Matchers {

  "A Bernoulli distribution" when {

    "𝙿 = 1" should {
      "yield a success" in {
        DiscreteDistribution.Bernoulli(1).get shouldBe true
      }
    }

    "𝙿 = 0" should {
      "yield a failure" in {
        DiscreteDistribution.Bernoulli(0).get shouldBe false
      }
    }
  }

  "A discrete uniform distribution" when {
    
    object Outcome

    "its sample space is empty" should {
      "throw an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          DiscreteDistribution.uniform()
        }
      }
    }

    "its sample space is a singleton" should {
      val d = Random.nextDouble
      val gen = DiscreteDistribution.uniform(d)
      "have equal mean and outcome" in {
        gen.get shouldBe d
        gen.mean shouldBe d
      }
      "have a standard deviation equal to the fourth power of the outcome" in {
        // Approximately equal
        (gen.standardDeviation - Math.pow(d, 4)) should be < 0.00001
      }
    }

    "its sample space includes multiple values" should {
      val gen = DiscreteDistribution.uniform(0.2, 1.6)
      "have a weighted mean" in {
        gen.mean shouldBe 0.9
      }
      "have a standard deviation" in {
        gen.standardDeviation shouldBe 0.49
      }
      "have a variance" in {
        gen.variance shouldBe 0.7
      }
    }
  }
}
