package com.github.neysofu
package tyche

import org.scalatest.{WordSpec, Matchers}
import shapes.Gauss

class GaussSpec extends WordSpec with Matchers {

  "A normal distribution" when {
    
    "μ = ∞" should {
      "yield ∞" in {
        Gauss(1.0 / 0, 1)().isInfinity shouldBe true
      }
    }

    "σ² = 0" should {
      "yield μ" in {
        Gauss(1, 0)() shouldBe 1
      }
    }

    "σ² < 0" should {
      "raise an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          Gauss(1, -1)
        }
      }
    }
  }
}
