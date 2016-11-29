package com.github.neysofu
package tyche
package algorithms

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class UniformSpec extends WordSpec with Matchers {

  "A uniform distribution" when {
    
    "𝑏 is greater than 𝑎" should {
      "throw an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          Uniform(1, 0)
        }
      }
    }

    "𝑏 equals 𝑎" should {
      val a = 1
      val uniform = Uniform(a, a)
      "have a null deviation" in {
        uniform.stdDeviation shouldBe 0.0
      }
      "have a mean equal to 𝑎 and 𝑏" in {
        uniform.mean shouldBe a
      }
    }
  }
}
