package com.github.neysofu
package tyche

import org.scalatest.{WordSpec, Matchers}

class DiscreteAutomatonSpec extends WordSpec with Matchers {

  val coin = DiscreteAutomaton(Map(0->1, 1->1))

  "A discrete distribution" when {
  
    "degenerate" should {
      val automaton = DiscreteAutomaton(Map(1 -> 1))
      "have no deviation" in {
        automaton.stdDeviation shouldBe 0
      }
      "have a mean equal to the only element" in {
        automaton.mean shouldBe 1
      }
    }

    "uniform" should {
      "have equal probabilities" in {
        coin probabilityOf (_ == 0) shouldBe .5
        coin probabilityOf (_ == 1) shouldBe .5
      }
    }

    "zipped with itself" should {
      "take O(n^2) space" in {
        (coin zip coin) probabilityOf (_ == (0, 0)) shouldBe .25
      }
    }

    "filtered by identity" should {
      "become degenerate" in {
        (coin filter (_ == 1))() shouldBe 1
      }
    }
  }

  "A probability mass function" when {

    "empty" should {
      "raise an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          DiscreteAutomaton(Map())
        }
      }
    }

    "containing negative probabilities" should {
      "raise an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
          DiscreteAutomaton(Map(1 -> -1, 2 -> 2))
        }
      }
    }
  }
}
