package io.neysofu.tyche

import scala.util.Random
import io.neysofu.tyche.util.Util
import org.scalatest.{WordSpec, Matchers}

class UniformSpec extends WordSpec with Matchers {

  val gen = ContinuousDistribution.uniform
  val gen2 = new ContinuousDistribution(() => Util.square(Random.nextDouble))

  "A uniform probability distribution" when {
    
    "sampled" should {
      "generate values in the ´[0,1[´ interval" in {
        gen.take(gen.sampleSize).forall(x => x >= 0 && x < 1)
        println(Random.nextDouble)
      }
    }
  }
}
