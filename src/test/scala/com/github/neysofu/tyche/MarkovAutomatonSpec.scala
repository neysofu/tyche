package com.github.neysofu
package tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class MarkovAutomatonSpec extends WordSpec with Matchers {

  sealed trait State
  object S1 extends State
  object S2 extends State

  val chain = MarkovAutomaton[State](S2, { case s => DiscreteAutomaton(Map(S1 -> 1))})
  
  "A Markov chain" when {
    
    "it enters an absoring state" should {    
      "be degenerate" in {
        chain.rule(chain.state).probabilityOf(_ != S1) shouldBe 0.0
      }
    }

    "modelled on \"11\"" should {
      "yield '1'" in {
        MarkovAutomaton.modelOn("11" : _*)().state shouldBe '1'
      }
    }
  }
}
