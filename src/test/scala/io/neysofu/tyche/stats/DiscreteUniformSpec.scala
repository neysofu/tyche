package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class DiscreteUniformSpec extends WordSpec with Matchers {

  //println(DiscreteGen.probabilityMassFunct(Seq((0.5, 1), (0.5, 2))).weights)
  val distr = Commons.newDiscreteUniform(Seq(1, 2))

  "A discrete uniform probability distribution" when {
    "sampled" should {
      "generate random outcomes according to a mass function" in {
        distr.get shouldBe a [Integer]
      }
    }
  }
}
