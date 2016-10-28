package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class GaussSpec extends WordSpec with Matchers {

  "A normal distribution" when {
   
    val sd = 1
    val ev = 0

    s"when ´SD=$sd´ and ´EV=$ev´" should {
      val gen = Commons.newGauss(sd, ev)
      "have a sample mean of ~0" in {
        Math.round(gen.mean) shouldBe 0.toLong
      }
    }
  }
}
