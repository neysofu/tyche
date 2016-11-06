package io.neysofu.tyche

import scala.util.Random

/** This trait defines fundamental data manipulation techniques for random
 *  variables and stochastic processes such as probability distributions.
 *
 *  Each and every instance is implemented by specifying its generative
 *  function ([[io.neysofu.tyche.Gen.get]]).
 *
 *  @example {{{
 *  val uniform = new Gen[Double] {
 *    def get = scala.util.Random.nextDouble
 *  }
 *  }}}
 *
 *  @tparam A the outcomes' type signature. Note that many implementions
 *  require `A <: Double` to work correctly.
 *  
 *  @see [[io.neysofu.tyche.Moments]]
 */
trait Gen[+A] { self =>

  /** Returns a random outcome.
   *
   *  @example {{{
   *  println(uniform.get)
   *  // 0.677464655118979
   *  }}}
   */
  def get: A

  /** Builds a new generator by applying a function to all the outcomes.
   *  
   *  @example {{{
   *  println(uniform.map(d => d + 1).get)
   *  // 1.742019032131262
   *  }}}
   */
  def map[B](f: A => B): Gen[B] = new Gen[B] {
    def get = f(self.get)
  }

  /** Builds a new generator by filtering the sample space accordingly to a
   *  predicate.
   *
   *  @example {{{
   *  println(uniform.filter(d => d > 0.8).get)
   *  // 0.8619005365066574
   *  }}}
   */
  def filter(pred: A => Boolean): Gen[A] = map { x =>
    def pickAnother(g: A): A = if (pred(g)) g else pickAnother(get)
    pickAnother(x)
  }

  /** Builds a new generator by creating a sample space which contains
   *  only collections of outcomes that satisfy a predicate.
   *
   *  @example {{{
   *  println(uniform.until(_.sum > 1).get)
   *  // Seq(0.45045141067752825, 0.35965061961514433, 0.3274988472269684)
   *  }}}
   */
  def until(pred: Seq[A] => Boolean): Gen[Seq[A]] = map { x =>
    def grow(ls: Seq[A]): Seq[A] = if (pred(ls)) ls else grow(ls :+ get)
    grow(Seq(x))
  }

  /** Builds a new generator by joining multiple outcomes together.
   *
   *  @example {{{
   *  println(uniform.repeat(2).get)
   *  // Seq(0.4656608004451568, 0.2043631751083831)
   *  }}}
   */
  def repeat(n: Int): Gen[Seq[A]] = map(_ +: take(n-1))

  /** Builds a new, bivariate generator by zipping the sample space with
   *  another generator's.
   *
   *  @example {{{
   *  println(uniform.joint(uniform.map(-_)).get)
   *  // (0.5872605617066919, -0.5959848796935293)
   *  }}}
   */
  def joint[B](that: Gen[B]): Gen[(A, B)] = map((_, that.get))

  /** Builds a new generator by replacing all the outcomes with their
   *  respective numerical representations.
   */
  def toGenDouble(implicit d: A <:< Double): Gen[Double] = map(d(_))

  /** Returns an array of the desired length filled with random outcomes.
   */
  def take(n: Int): Seq[A] = Seq.fill(n)(get)
  
  /** Returns a stream of outcomes.
   *
   *  @example {{{
   *  println(uniform.stream)
   *  // Stream(0.6295245393412436, ?)
   *  }}}
   */
  def toStream: Stream[A] = get #:: toStream
}  
