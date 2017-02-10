package com.github.neysofu
package tyche

import org.scalatest.{WordSpec, Matchers}
import shapes.ChiSquare

class ChiSquareSpec extends WordSpec with Matchers {

  "A chi-squared distribution" when {
   
    "𝑘 < 0" should {
      "raise an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          ChiSquare(-1)
        }
      }
    }

    "𝑘 = 0" should {
      "yield 0" in {
        ChiSquare(0)() shouldBe 0
      }
    }

    "𝑘 > 0" should {
      "yield a nonnegative number" in {
        ChiSquare(1)() should be >= 0.0
      }
    }
  }
}
