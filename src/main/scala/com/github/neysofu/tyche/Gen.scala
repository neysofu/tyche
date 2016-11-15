package com.github.neysofu.tyche

import scala.util.Random

/** A Tyche generator is a high-level description of random variables and
 *  stochastic processes. Each and every instance is implemented by specifying
 *  its generative function ([[com.github.neysofu.tyche.Gen.get]]).
 *
 *  @define hanging
 *  @note impossible predicates will result in the hanging of the program.
 *
 *  @tparam A the element type of this generator. Note that analysis methods
 *  require `A <: Double` in order to work.
 *  @see [[com.github.neysofu.tyche.Moments]]
 */
trait Gen[+A] {
    self =>

  override def toString = "<generator>"

  /** Computes a value. This method is known as the ''generative function''
   *  of this generator.
   *
   *  @return a value.
   */
  def get: A

  /** Builds a new generator by applying a function to each value.
   *  
   *  @tparam B the range type of the given function `f`.
   *  @param f the function to apply to each value.
   *  @return a new generator resulting from replacing each value with the
   *  output of the given function `f` with it as an input.
   */
  def map[B](f: A => B): Gen[B] = new Gen[B] {
    def get = f(self.get)
  }

  /** Builds a new generator by filtering values with a predicate.
   *
   *  @param pred the predicate that each value must satisfy.
   *  @return a new generator resulting from picking only values that satify
   *  the given predicate `pred`.
   *  $hanging
   */
  def filter(pred: A => Boolean): Gen[A] = map { x =>
    def rec(g: A): A = if (pred(g)) g else rec(get)
    rec(x)
  }

  /** Builds a new generator by creating sequences of values which satisfy a
   *  predicate.
   *
   *  @param pred the predicate that each list must satisfy.
   *  @return a new generator of lists resulting from appending values to an
   *  empty list until it satisfies the given predicate `pred`.
   *  $hanging
   */
  def until(pred: List[A] => Boolean): Gen[List[A]] = map { x =>
    def rec(ls: List[A]): List[A] = if (pred(ls)) ls else rec(ls :+ get)
    rec(List(x))
  }

  /** Builds a new generator by filling sequences with values.
   *
   *  @param n the length of each list.
   *  @return a new generator resulting from replacing each value with a
   *  `n`-sized list of values.
   */
  def repeat(n: Int): Gen[Seq[A]] = map(take(n-1) :+ _)

  /** Builds a new bivariate generator by coupling this generator with
   *  another.
   *
   *  @tparam B the element type of the given generator `that`.
   *  @param that the generator to zip with this generator.
   *  @return a new generator resulting from zipping each value with one of
   *  the given generator `that`.
   */
  def joint[B](that: Gen[B]): Gen[(A, B)] = map((_, that.get))

  /** Builds a new generator by replacing each value with its numerical
   *  representation.
   *
   *  @return a new generator resulting from replacing each value with its
   *  floating point number representation.
   */
  def toGenDouble(implicit d: A <:< Double): Gen[Double] = map(d(_))

  /** Computes a sequence filled with values.
   *
   *  @param n the length of the returned list.
   *  @return a `n`-sized list filled with values.
   */
  def take(n: Int): Seq[A] = Seq.fill(n)(get)
  
  /** Computes a stream of values.
   *
   *  @return a stream of values.
   */
  def toStream: Stream[A] = get #:: toStream
}  
