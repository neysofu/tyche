package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class UniformSpec extends WordSpec with Matchers {

  val gen = ContinuousGen.uniform()

  "A uniform probability distribution" when {
    
    "sampled" should {
      "generate values in the ´[0,1[´ interval" in {
        gen.times(gen.sampleSize).forall(x => x >= 0 && x < 1)
      }
    }
  }
}
