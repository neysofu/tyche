package com.github.neysofu.tyche

import scala.util.Random

/** A Tyche generator is a high-level descriptcom.github. of random variables and
 *  stochastic processes.
 *
 *  Each and every instance is implemented by specifying its generative
 *  functcom.github. ([[com.github.neysofu.tyche.Gen.get]]).
 *
 *  @example {{{
 *  scala> val uniform = new Gen[Double] {
 *       |   def get = scala.util.Random.nextDouble
 *       | }
 *  uniform: com.github.neysofu.tyche.Gen[Double] = \$...
 *
 *  }}}
 *
 *  @tparam A the outcomes' type signature. Note that many implementcom.github.s
 *  require `A <: Double` to work correctly.
 *  
 *  @see [[com.github.neysofu.tyche.Moments]]
 */
trait Gen[+A] { self =>

  /** Returns a random outcome.
   *
   *  @example {{{
   *  scala> uniform.get
   *  res1: Double = 0.9749134509667129
   *
   *  }}}
   */
  def get: A

  /** Builds a new generator by applying a functcom.github. to all the outcomes.
   *  
   *  @example {{{
   *  scala> (uniform map (_+1)).get
   *  res1: Double = 1.9153124129473624
   *
   *  }}}
   */
  def map[B](f: A => B): Gen[B] = new Gen[B] {
    def get = f(self.get)
  }

  /** Builds a new generator by filtering the sample space accordingly to a
   *  predicate.
   *
   *  @example {{{
   *  scala> (uniform filter (_ > 0.8)).get
   *  res1: Double = 0.8433116704232428
   *  
   *  }}}
   */
  def filter(pred: A => Boolean): Gen[A] = map { x =>
    def pickAnother(g: A): A = if (pred(g)) g else pickAnother(get)
    pickAnother(x)
  }

  /** Builds a new generator by creating a sample space which contains
   *  only collectcom.github.s of outcomes that satisfy a predicate.
   *
   *  @example {{{
   *  scala> (uniform until (_.sum > 1)).get
   *  res1: Seq[Double] = List(0.8456141535917049, 0.5993270795832424)
   *  
   *  }}}
   */
  def until(pred: Seq[A] => Boolean): Gen[Seq[A]] = map { x =>
    def grow(ls: Seq[A]): Seq[A] = if (pred(ls)) ls else grow(ls :+ get)
    grow(Seq(x))
  }

  /** Builds a new generator by joining multiple outcomes together.
   *
   *  @example {{{
   *  scala> uniform.repeat(2).get
   *  res1: Seq[Double] = List(0.39691936209812506, 0.8606695143524133)
   *  
   *  }}}
   */
  def repeat(n: Int): Gen[Seq[A]] = map(_ +: take(n-1))

  /** Builds a new bivariate generator by zipping the sample space with
   *  another generator's.
   *
   *  @example {{{
   *  scala> uniform.joint(uniform map (-_)).get
   *  res1: (Double, Double) = (0.6916518719652441,-0.45253449818494484)
   *  
   *  }}}
   */
  def joint[B](that: Gen[B]): Gen[(A, B)] = map((_, that.get))

  /** Builds a new generator by replacing all the outcomes with their
   *  respective numerical representatcom.github.s.
   */
  def toGenDouble(implicit d: A <:< Double): Gen[Double] = map(d(_))

  /** Returns an array of the desired length filled with random outcomes.
   *
   *  @example {{{
   *  scala> uniform.take(2)
   *  res1: Seq[Double] = List(0.3562246063380463, 0.4851991867143829)
   *  
   *  }}}
   */
  def take(n: Int): Seq[A] = Seq.fill(n)(get)
  
  /** Returns a stream of outcomes.
   *
   *  @example {{{
   *  scala> uniform.toStream
   *  res1: Stream[Double] = Stream(0.7792234677947281, ?)
   *  
   *  }}}
   */
  def toStream: Stream[A] = get #:: toStream
}  
