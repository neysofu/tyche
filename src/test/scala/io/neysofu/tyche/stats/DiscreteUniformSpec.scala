package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class DiscreteUniformSpec extends WordSpec with Matchers {

  val distr = Commons.newDiscreteUniform("random")

  "A discrete uniform probability distribution" when {
    "sampled" should {
      "generate random outcomes according to a mass function" in {
        distr.get shouldBe a [Character]
      }
    }
  }
}
