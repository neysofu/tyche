package io.neysofu.tyche
package stats

import org.scalatest._

class BinomialSpec extends FlatSpec with Matchers {
  val binomial = Commons.newBinomial(24, 0.5)

  "A binomial distribution" should "give integer results" in {
    val d = binomial.get
    d should equal (Math.floor(d))
  }
}
