package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class BernoulliSpec extends WordSpec with Matchers {

  val distr = Commons.newBernoulli(1)

  "A Bernoulli distribution (p=1)" when {
    "sampled" should {
      "always yield 1" in {
        distr.get shouldBe true
      }
    }
  }
}
