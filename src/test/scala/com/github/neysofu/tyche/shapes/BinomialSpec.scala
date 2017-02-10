package com.github.neysofu
package tyche

import org.scalatest.{WordSpec, Matchers}
import shapes.Binomial

class BinomialSpec extends WordSpec with Matchers {

  "A binomial distribution" when {

    val n = 3

    "𝑝 = 1" should {
      "yield 𝑛" in {
        Binomial(n, 1)() shouldBe n
      }
    }

    "𝑝 = 0" should {
      "yield 0" in {
        Binomial(n, 0)() shouldBe 0
      }
    }

    "𝑛 = 0" should {
      "yield 0" in {
        Binomial(0, 1)() shouldBe 0
      }
    }

    "𝑛 < 0" should {
      "raise an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          Binomial(-1, 1)
        }
      }
    }
  }
}
