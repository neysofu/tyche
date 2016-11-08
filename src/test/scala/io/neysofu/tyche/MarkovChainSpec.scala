package io.neysofu.tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}

class MarkovChainSpec extends WordSpec with Matchers {

  sealed trait State
  object FinalState extends State
  object StartState extends State

  val chain = new MarkovChain[State](
    _ match {
      case FinalState => DiscreteDistribution(Map(FinalState -> 1.0))
      case StartState => DiscreteDistribution(Map(FinalState -> 1.0))
    },
    StartState
  )
  
  "A Markov chain" when {
    
    "it has an absorbing state" should {    
      "maintain the same state at each iteration" in {
        chain.get.state shouldBe chain.get.get.state
      }
    }

    "modelled on \"11\"" should {
      "yield '1'" in {
        MarkovChain.modelOn("11" : _*).get.state shouldBe '1'
      }
    }
  }
}
