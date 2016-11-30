package com.github.neysofu
package tyche

import scala.util.Random
import scala.collection.mutable.Queue
import scala.annotation.tailrec

/** A generic trait for generators.
 *
 *  A generator is a high-level description of random variables and stochastic
 *  processes. Each and every instance is implemented by specifying its
 *  generative function ([[com.github.neysofu.tyche.Gen.get]]).
 *
 *  @define hanging
 *  @note impossible predicates may result in the hanging of the program.
 *
 *  @tparam A the element type of this generator. Note that analysis methods
 *  require `A <: Double` in order to work.
 */
trait Gen[+A] extends Function0[A] {
    self =>

  /** Builds a new generator by applying a function to each value.
   *  
   *  @tparam B the range type of the given function `f`.
   *  @param f the function to apply to each value.
   *  @return a new generator resulting from replacing each value with the
   *  output of the given function `f` with it as an input.
   */
  final def map[B](f: A => B): Gen[B] = Gen(f(apply))

  def flatMap[B](f: A => Seq[B]): Gen[B] =
    map(x => Random.shuffle(f(x)).head)

  /** Builds a new generator by filtering values with a predicate.
   *
   *  @param pred the predicate that each value must satisfy.
   *  @return a new generator resulting from picking only values that satisfy
   *  the given predicate `pred`.
   *  $hanging
   */
  def filter(p: A => Boolean): Gen[A] = map {
    x => var a = x
    while (!p(a)) a = apply
    a
  }

  def filterNot(p: A => Boolean): Gen[A] = filter (!p(_))

  /** Builds a new generator by creating sequences of values which satisfy a
   *  predicate.
   *
   *  @param pred the predicate that each list must satisfy.
   *  @return a new generator of lists resulting from appending values to an
   *  empty list until it satisfies the given predicate `pred`.
   *  $hanging
   */
  def until(p: Seq[A] => Boolean): Gen[Seq[A]] = map {
    x => var ls = Queue(x)
    while (!p(ls)) ls += apply
    ls
  }

  def zip[B](that: Gen[B]): Gen[(A, B)] = map (a => (a, that.apply))

  /** The number of observations to include in a statistical sample.
   *
   *  The default value assures a confidence level of 95% and a margin of
   *  error of 0.01 for distributions with a normal (0.5 or less) standard
   *  deviation.
   */
  protected val sampleSize: Int = 10000

  def take(n: Int): Seq[A] = Seq.fill(n)(apply)

  def sample: Seq[A] = take(sampleSize)

  /** Computes the probability of an event.
   *
   *  @param p the event to analyse.
   *  @return the probability of the given event `event`.
   */
  def probabilityOf(p: A => Boolean): Double =
    sample.count(p) / sampleSize
  
  /** Computes the [[https://goo.gl/LruXGw expected value]] (mean).
   *
   *  @return the expected value.
   */
  def mean(implicit toDouble: A => Double): Double =
    sample.map(toDouble(_) / sampleSize).sum
  
  /** Computes the [[https://goo.gl/QrSlFY standard deviation]].
   *
   *  @return the standard deviation.
   */
  def stdDeviation(implicit toDouble: A => Double): Double = {
    val m = mean
    (sample map (x => (x*x - m) / sampleSize)).sum
  }
  
  /** Computes the [[https://goo.gl/Wzlr6p variance]].
   *
   *  @return the variance.
   */
  def variance(implicit toDouble: A => Double): Double =
    Math.pow(stdDeviation, 2)

  /** Computes a stream of values.
   *
   *  @return a stream of values.
   */
  final def toStream: Stream[A] = apply #:: toStream
}

/** This object contains the builders for `Gen` instances.
 */
object Gen {

  /** Builds a new generator by setting its generative function.
   *
   *  @tparam A the element type of the returned generator.
   *  @param f the generative function of the returned generator.
   *  @return a new generator with generative function `f`.
   */
  def apply[A](f: => A): Gen[A] = new Gen[A] {
    def apply: A = f
  }
}
