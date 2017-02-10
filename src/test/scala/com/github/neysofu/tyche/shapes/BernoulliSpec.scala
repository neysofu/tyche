package com.github.neysofu
package tyche

import org.scalatest.{WordSpec, Matchers}
import shapes.Bernoulli

class BernoulliSpec extends WordSpec with Matchers {

  "A Bernoulli distribution" when {

    "𝑝 = 1" should {
      "yield a success" in {
        Bernoulli(1)() shouldBe true
      }
    }

    "𝑝 = 0" should {
      "yield a failure" in {
        Bernoulli(0)() shouldBe false
      }
    }

    "𝑝 is outside bounds" should {
      "throw an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          Bernoulli(-1)
        }
        a [java.lang.IllegalArgumentException] should be thrownBy {
          Bernoulli(2)
        }
      }
    }
  }
}
