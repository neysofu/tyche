package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class UniformSpec extends WordSpec with Matchers {

  val distr = Commons.newUniform()

  "A uniform probability distribution" when {
    "sampled" should {
      "generate values in the [0, 1] range" in {
        distr.get should be >=0.0
        distr.get should be <=1.0
      }
    }
  }
}

