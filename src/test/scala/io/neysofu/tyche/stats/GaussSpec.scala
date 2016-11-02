package io.neysofu.tyche

import org.scalatest.{WordSpec, Matchers}

class GaussSpec extends WordSpec with Matchers {

  val sd = 1
  val ev = 0
  val gen = ContinuousGen.normal(sd, ev)

  "A normal distribution" when {
   
    f"when ´SD=$sd´ and ´EV=$ev´" should {
      "have a sample mean of ~0" in {
        Math.round(gen.mean) shouldBe 0.toLong
      }
    }
  }
}
