package com.github.neysofu.tyche

import org.scalatest.{WordSpec, Matchers}
import algorithms._

class DiscreteGenSpec extends WordSpec with Matchers {

  val zero = 0
  val one = 1

  val double = 0.123

  "A Bernoulli distribution" when {

    "𝑝 = 1" should {
      "yield a success" in {
        Bernoulli(one)() shouldBe true
      }
    }

    "𝑝 = 0" should {
      "yield a failure" in {
        Bernoulli(zero)() shouldBe false
      }
    }
  }

  "A discrete uniform distribution" when {
    
    "its sample space is empty" should {
      "throw an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
         DiscreteUniform()()
        }
      }
    }

    "its sample space is a singleton" should {
      val gen = DiscreteUniform(double)
      "have equal mean and value" in {
        gen() shouldBe double
        gen.mean shouldBe double
      }
    }

    "its sample space includes multiple values" should {
      val double1 = double
      val double2 = double
      val gen = Uniform(double1, double2)
      "be able to compute the weighted mean" in {
        gen.mean shouldBe (double1 + double2) / 2
      }
    }
  }

  "A binomial distribution" when {

    val n = 3

    "𝑝 = 1" should {
      "yield 𝑛" in {
        Binomial(n, one)() shouldBe n
      }
    }

    "𝑝 = 0" should {
      "yield 0" in {
        Binomial(n, zero)() shouldBe zero
      }
    }

    "sampled" should {
      "yield a nonnegative value" in {
        Binomial(n, double)() should be >= 0
      }
    }
  }
}
