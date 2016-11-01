package io.neysofu.tyche

import org.scalatest.{WordSpec, Matchers}

class ChiSquareSpec extends WordSpec with Matchers {

  val gen = ContinuousGen.chiSquare(1)

  "A chi-square probability distribution" when {
    
    "sampled" should {
      "generate nonnegative values" in {
        gen.get should be >= 0.0
      }
    }
  }
}
