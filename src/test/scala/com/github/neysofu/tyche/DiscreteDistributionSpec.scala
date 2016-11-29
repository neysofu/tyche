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
          uniform(Seq() :_*)() // Ugly asf.
        }
      }
    }

    "its sample space is a singleton" should {
      val gen = uniform(double)
      "have equal mean and value" in {
        gen() shouldBe double
        gen.mean shouldBe double
      }
    }

    "its sample space includes multiple values" should {
      val double1 = double
      val double2 = double
      val gen = uniform(double1, double2)
      "be able to compute the weighted mean" in {
        gen.mean shouldBe (double1 + double2) / 2
      }
    }
  }

  "A binomial distribution" when {

    val n = 57

    "𝑝 = 1" should {
      "yield 𝑛" in {
        binomial(n, one)() shouldBe n
      }
    }

    "𝑝 = 0" should {
      "yield 0" in {
        binomial(n, zero)() shouldBe zero
      }
    }

    "sampled" should {
      "yield a nonnegative value" in {
        binomial(n, double)() should be >= 0
      }
    }
  }
}
