package com.github.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

import algorithms._

class ContinuousGenSpec extends WordSpec with Matchers {

  "A normal distribution" when {
    
    "μ = ∞" should {
      "ignore σ²" in {
        Gauss(1.0 / 0.0, 1)().isInfinity shouldBe true
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
        chiSquare(0)() shouldBe 0
      }
    }

    "k > 0" should {
      val gen = chiSquare(1)
      "yield a nonnegative value" in {
        gen() should be >= 0.0
      }
      "have a nonnegative mean" in {
        gen.mean should be >= 0.0
      }
    }
  }

  "A continuous uniform distribution" when {
    
    "sampled" should {
      "yield a value that falls within the ´[0, 1[´ interval" in {
        val outcome = uniform()
        outcome should be < 1.0
        outcome should be >= 0.0
      }
    }
  }

  "A pseudo-continuous distribution" when {
    
    val gen = Gen(1.0)
    
    "its sample space size equals 1" should {
      "have equal mean and outcome" in {
        (gen() - gen.mean).abs should be < 0.0001
      }
      "have a null standard deviation" in {
        gen.stdDeviation.abs should be < 0.0001
      }
    }
  }
}
