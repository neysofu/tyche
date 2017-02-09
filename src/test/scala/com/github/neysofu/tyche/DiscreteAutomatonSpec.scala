package com.github.neysofu
package tyche

import scala.util.Random
import org.scalatest.{WordSpec, Matchers}
import implicits.EqualityImplicits._
import shapes._

class DiscreteAutomatonSpec extends WordSpec with Matchers {

  def rndDouble: Double = Random.nextDouble

  "A Bernoulli distribution" when {

    "𝑝 = 1" should {
      "yield a success" in {
        Bernoulli(1)() shouldBe true
      }
    }

    "𝑝 = 0" should {
      "yield a failure" in {
        Bernoulli(0)() shouldBe false
      }
    }
  }

  "A discrete uniform distribution" when {
    
    "its sample space is empty" should {
      "throw an exception" in {
        a [java.lang.IllegalArgumentException] should be thrownBy {
         DiscreteAutomaton.uniform()()
        }
      }
    }

    "its sample space is a singleton" should {
      "have equal mean and value" in {
        val d = rndDouble
        val auto = DiscreteAutomaton.uniform(d)
        auto() shouldBe d
        auto.mean shouldBe d
      }
    }

    "its sample space includes multiple values" should {
      "be able to compute the weighted mean" in {
        val int = Random.nextInt(42)
        val mean = DiscreteAutomaton.uniform(0, int).mean
        assert(mean === int.toDouble / 2)
      }
      "zip deterministically" in {
        val die = DiscreteAutomaton.uniform(1, 6)
        val dice = die zip die
        val best = dice map (p => Math.max(p._1, p._2))
        assert((best probabilityOf (_ == 1)) === 1.0 / 36)
      }
    }
  }

  "A binomial distribution" when {

    val n = 3

    "𝑝 = 1" should {
      "yield 𝑛" in {
        Binomial(n, 1)() shouldBe n
      }
    }

    "𝑝 = 0" should {
      "yield 0" in {
        Binomial(n, 0)() shouldBe 0
      }
    }

    "sampled" should {
      "yield a nonnegative value" in {
        Binomial(n, rndDouble)() should be >= 0
      }
    }
  }
}
