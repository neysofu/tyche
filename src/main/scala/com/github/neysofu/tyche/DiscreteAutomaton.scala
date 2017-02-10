package com.github.neysofu
package tyche

import scala.util.Random
import scala.collection.mutable.{Map => Dict}
import generators.{MassTraitFunction, MassFunction}

trait DiscreteAutomaton[A] extends Automaton[A] with MassTraitFunction[A] {
    self =>

  override def map[B](f: A => B): Automaton[B] = {
    val dict = Dict.empty[B, Double]
    for (kv <- mass) yield {
      val key = f(kv._1)
      if (!(dict contains key))
        dict(key) = 0
      dict(key) += kv._2
    }
    DiscreteAutomaton(dict.toMap)
  }

  override def filter(p: A => Boolean): Automaton[A] =
    DiscreteAutomaton(mass filter (kv => p(kv._1)))

  override def probabilityOf(p: A => Boolean): Double =
    (mass filter (a => p(a._1))).values.sum / weights.last

  override def mean(implicit toDouble: A => Double): Double =
    (mass map {case (k, v) => toDouble(k) * v}).sum / weights.last

  override def stdDeviation(implicit toDouble: A => Double): Double = {
    val m = mean
    def diff(k: A) = Math.pow(toDouble(k)-m, 2)
    (mass map {case (k, v) => diff(k) * v}).sum / weights.last
  }

  override def zip[B](that: Automaton[B]): Automaton[(A, B)] = that match {
    case da: DiscreteAutomaton[B] => {
      val seq = for {
        a <- this.mass
        b <- da.mass
      } yield (a._1, b._1) -> a._2 * b._2
      DiscreteAutomaton(seq.toMap)
    }
    case _ => super.zip(that)
  }
}

object DiscreteAutomaton {

  def apply[A](mass: Map[A, Double]): DiscreteAutomaton[A] = {
    MassFunction(mass)
    val m = mass
    new DiscreteAutomaton[A] {
      val mass: Map[A, Double] = m
    }
  }

  /** Builds a new discrete uniform distribution.
   * 
   *  @param values the sample space.
   *  @return a discrete uniform distribution with the given sample space
   *  `values`.
   */
  def uniform[A](space: A*): DiscreteAutomaton[A] = {
    DiscreteAutomaton((space map (x => x -> 1.0)).toMap)
  }

  def uniform(n: Int, m: Int): DiscreteAutomaton[Int] =
    uniform(n to m :_*)
}
