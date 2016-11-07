package io.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class MarkovChainSpec extends WordSpec with Matchers {

  sealed trait State
  object FinalState extends State
  object StartState extends State

  val chain = new MarkovChain[State](
    _ match {
      case FinalState => Map(FinalState -> 1.0)
      case StartState => Map(FinalState -> 1.0)
    }, StartState
  )
  
  "A Markov chain" when {
    
    "it has an absorbing state" should {    
      "maintain the same state at each iteration" in {
        chain.get.state shouldBe chain.get.get.state
      }
    }

    "extracted from ´\"--\"´" should {
      "yield ´-´" in {
        MarkovChain.estimateFrom("--" : _*).get.state shouldBe '-'
      }
    }
  }
}
