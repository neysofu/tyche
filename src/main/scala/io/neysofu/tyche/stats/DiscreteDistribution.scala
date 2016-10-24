package io.neysofu.tyche
package stats

import scala.util.Random

/** Represents a discrete probability distribution.
 *
 *  Resources
 *  ---------
 *  1. https://www.youtube.com/watch?v=3MOahpLxj6A
 */
abstract class DiscreteDistribution[A] extends Distribution[A] { self =>

  override def map[B](f: A => B) = new DiscreteDistribution[B] {
    val mass = self.mass.map(x => (x._1, f(x._2)))
  }

  private val cumSum: MassFunction[A] = mass.map {
    var s = 0.0; d => { s += d._1; (s, d._2) }
  }
 
  def get = {
    val d = Random.nextDouble()
    cumSum.find(_._1 > d).getOrElse(cumSum(0))._2
  }

  /** The probability mass function of the probability distribution.
   */
  type MassFunction[T] = Seq[(Double, T)]
  val mass: MassFunction[A]
 }
