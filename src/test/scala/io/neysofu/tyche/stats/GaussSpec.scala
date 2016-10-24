package io.neysofu.tyche
package stats

import org.scalatest._

class GaussSpec extends FlatSpec with Matchers {

  "A normal distribution" should "have an expected value of 0" in {
    val distr = new Gauss()
    distr.mean should be > -1.0
    distr.mean should be < +1.0
  }
}
