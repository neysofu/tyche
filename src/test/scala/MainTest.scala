package io.neysofu.tyche
package stats

import org.scalatest._

class DistributionSpec extends FlatSpec with Matchers {

  "A normal distribution" should "have an expected value of 0" in {
    val distr = new Gauss(1, 1)
    distr.mean should be > 0.0
    distr.mean should be < 2.0
  }
}
