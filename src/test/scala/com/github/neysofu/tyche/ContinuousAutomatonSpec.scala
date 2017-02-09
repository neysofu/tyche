package com.github.neysofu
package tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}
import shapes._
import implicits.EqualityImplicits._
import util.Interval

class AutomatonSpec extends WordSpec with Matchers {

  "A normal distribution" when {
    
    "μ = ∞" should {
      "ignore σ²" in {
        Gauss(1.0 / 0, 1)().isInfinity shouldBe true
      }
    }

    "σ² = 0" should {
      "yield μ" in {
        Gauss(1, 0)() shouldBe 1
      }
    }
  }

  "A chi-squared distribution" when {
    
    "k = 0" should {
      "yield 0" in {
        ChiSquare(0)() shouldBe 0
      }
    }

    "k > 0" should {
      val cq1 = ChiSquare(1)
      "yield a nonnegative value" in {
        cq1() should be >= 0.0
      }
      "have a nonnegative mean" in {
        cq1.mean should be >= 0.0
      }
    }
  }

  "A continuous uniform distribution" when {
    
    "sampled" should {
      "yield a value that falls within the ´[0, 1[´ interval" in {
        val outcome = Uniform(Interval(0, 1))
        outcome() should be < 1.0
        outcome() should be >= 0.0
      }
    }
  }

  "A pseudo-continuous distribution" when {
    
    "its sample space size equals 1" should {
      val auto = Automaton(1.0)
      "have equal mean and outcome" in {
        assert(auto() === auto.mean)
      }
      "have a null standard deviation" in {
        assert(auto.stdDeviation === 0.0)
      }
    }
  }
}
