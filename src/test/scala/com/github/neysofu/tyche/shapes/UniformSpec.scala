package com.github.neysofu
package tyche
package shapes

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}
import util.Interval

class UniformSpec extends WordSpec with Matchers {

  "A uniform distribution" when {
    
    "𝑏 is greater than 𝑎" should {
      "throw an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          Uniform(Interval(1, 0))
        }
      }
    }

    "𝑏 equals 𝑎" should {
      val a = 1
      val uniform = Uniform(Interval(a, a))
      "have a mean equal to 𝑎 and 𝑏" in {
        uniform.mean shouldBe a
      }
    }
  }
}
