package io.neysofu.tyche

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
      "be deterministic" in {
        DiscreteDistribution.uniform(Outcome).get shouldBe Outcome
      }
    }
  }
}
