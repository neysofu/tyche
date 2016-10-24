package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class GaussSpec extends WordSpec with Matchers {

  val distr = Commons.newGauss(1, 0)

  "A normal distribution" when {
    "sampled" should {
      "have an expected value of 0" in {
        distr.mean should be > -1.0
        distr.mean should be < +1.0
      }
    }
  }
}
