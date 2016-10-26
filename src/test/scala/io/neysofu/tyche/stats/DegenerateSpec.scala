package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class DegenerateSpec extends WordSpec with Matchers {

  val distr = Commons.newDegenerate(0)

  "A degenerate distribution" when {
    "sampled" should {
      "always return the same value" in {
        distr.get shouldBe 0
      }
    }
  }
}
