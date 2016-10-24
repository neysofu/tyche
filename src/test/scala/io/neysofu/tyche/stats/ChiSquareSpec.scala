package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class ChiSquareSpec extends WordSpec with Matchers {

  val distr = Commons.newChiSquare(4)

  "A chi-squared (k=3) probability distribution" when {
    "sampled" should {
      "generate nonnegative values" in {
        distr.get should be >= 0.0
      }
    }
  }
}
