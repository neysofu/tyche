package io.neysofu.tyche

import scala.util.Random

/** This trait defines fundamental data manipulation techniques for random
 *  variables and stochastic processes, such as probability distributions.
 *
 *  The type parameter `A` represents the outcomes' type signature. Note that
 *  many implementions require `A <: Double` to work correctly, with the
 *  notable exception of Markovian chains.
 *
 *  Each and every instance is implemented by specifying its generative
 *  function ([[io.neysofu.tyche.Gen.get]]), as so:
 *
 *  {{{
 *  val uniform = new Gen[Double] {
 *    def get = random.nextDouble
 *  }
 *  }}}
 *
 *  @author Filippo Costa
 *  @see [[io.neysofu.tyche.Moments]], [[io.neysofu.tyche.Markovian]]
 */
trait Gen[A] { self =>

  /** Returns a random outcome.
   */
  def get: A

  /** Builds a new generator by applying a function to all the outcomes.
   */
  def map[B](f: A => B): Gen[B] = new Gen[B] {
    def get = f(self.get)
  }

  /** Builds a new generator by filtering the sample space accordingly to a
   *  predicate.
   */
  def filter(pred: A => Boolean): Gen[A] = map { x =>
    def pickAnother(g: A): A = if (pred(g)) g else pickAnother(get)
    pickAnother(x)
  }

  /** Builds a new generator by creating a sample space which contains
   *  only collections of outcomes that satisfy a predicate.
   */
  def until(pred: Seq[A] => Boolean): Gen[Seq[A]] = map { x =>
    def grow(ls: Seq[A]): Seq[A] = if (pred(ls)) ls else grow(ls :+ get)
    grow(Seq(x))
  }

  /** Builds a new generator by joining multiple outcomes together.
   */
  def repeat(n: Int): Gen[Seq[A]] = map(_ +: take(n-1))

  /** Builds a new, bivariate generator by zipping the sample space with
   *  another generator's.
   */
  def joint[B](that: Gen[B]): Gen[(A, B)] = map((_, that.get))

  /** Builds a new generator by replacing all the outcomes with their
   *  respective numerical representations.
   */
  def toGenDouble(implicit d: A <:< Double): Gen[Double] = map(d(_))

  /** Returns an array of the desired length filled with random outcomes.
   */
  def take(n: Int): Seq[A] = Seq.fill(n)(get)
  
  /** Returns a [[Stream]] of outcomes.
   */
  def toStream: Stream[A] = get #:: toStream
}  
