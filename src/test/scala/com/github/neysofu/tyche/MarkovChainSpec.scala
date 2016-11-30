package com.github.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}
import algorithms._

class MarkovChainSpec extends WordSpec with Matchers {

  sealed trait State
  object S1 extends State
  object S2 extends State

  val chain = MarkovGen[State](
    { case _ => DiscreteGen(Map(S1 -> 1))},
    S2
  )
  
  "A Markov chain" when {
    
    "it enters an absoring state" should {    
      "be degenerate" in {
        chain.rule(chain.state).probabilityOf(_ != S1) shouldBe 0.0
      }
    }

    "modelled on \"11\"" should {
      "yield '1'" in {
        MarkovGen.modelOn("11" : _*)().state shouldBe '1'
      }
    }
  }
}
