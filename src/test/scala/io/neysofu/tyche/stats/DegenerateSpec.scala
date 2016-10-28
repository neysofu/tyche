package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class DegenerateSpec extends WordSpec with Matchers {

  case object Outcome
  val gen = Commons.newDegenerate(Outcome)

  "A degenerate distribution" when {
    
    "sampled" should {
      "always return the same value" in {
        gen.get shouldBe Outcome
      }
    }
  }
}
