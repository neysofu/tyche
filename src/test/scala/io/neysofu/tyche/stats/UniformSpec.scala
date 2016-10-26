package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class UniformSpec extends WordSpec with Matchers {

  val distr = Commons.newUniform()

  "A uniform probability distribution" when {
    "mapped" should {
      "return different outcomes" in {
        distr.map(x => x + 1).get should be >= 1.0
      }
    }
  }
}
