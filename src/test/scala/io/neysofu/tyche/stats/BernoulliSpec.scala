package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class BernoulliSpec extends WordSpec with Matchers {

  val p = 1
  val gen = Commons.newBernoulli(p)

  f"A Bernoulli distribution (´p=$p´)" when {
    
    "sampled" should {
      "always be successful" in {
        gen.get shouldBe true
      }
    }
  }
}
