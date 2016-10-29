package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class BernoulliSpec extends WordSpec with Matchers {

  val always = Commons.newBernoulli(1)

  "A Bernoulli distribution (´p=1´)" when {
    
    "sampled" should {
      "always be successful" in {
        always.get shouldBe true
      }
    }
  }
}
