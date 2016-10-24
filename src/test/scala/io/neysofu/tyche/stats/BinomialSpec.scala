package io.neysofu.tyche
package stats

import org.scalatest.{WordSpec, Matchers}

class BinomialSpec extends WordSpec with Matchers {
  
  val binomial = Commons.newBinomial(8, 0.6)

  "A binomial distribution" when {
    "sampled" should {
      "return an integer number" in {
        val d = binomial.get
        d should equal (Math.floor(d))
      }
    }
  }
}
