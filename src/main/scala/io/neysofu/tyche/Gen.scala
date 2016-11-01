package io.neysofu.tyche

import scala.util.Random

/** Represents a generic probability distribution. It supports sampling,
 *  plotting, data manipulation features, zipping, and more.
 */
trait Gen[A] {
  self =>

  /** A unique random variable should be used for debugging purposes.
   */
  val random: Random = new Random

  /** Returns a random outcome. This is said to be the generative
   *  characteristic function of the probability distribution.
   */
  def get: A

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is changed accordingly to a given
   *  function.
   */
  def map[B](f: A => B): Gen[B] = new Gen[B] {
    def get = f(self.get)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is shrunk accordingly to a given
   *  predicate.
   */
  def given(pred: A => Boolean): Gen[A] = map { x =>
    def repeat(g: A): A = {
      if (pred(g)) g
      else repeat(get)
    }
    repeat(x)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which only contains arrays of outcomes
   *  that satisfy a given predicate.
   */
  def until(pred: Seq[A] => Boolean): Gen[Seq[A]] = map { x =>
    def grow(ls: Seq[A]): Seq[A] = {
      if (pred(ls)) ls
      else grow(ls :+ get)
    }
    grow(Seq(x))
  }

  /** Returns an array of the desired length filled with random outcomes.
   */
  def times(n: Int): Seq[A] = Seq.fill(n)(get)

  /** Returns a new bivariate probability distribution originated from both
   *  the current instance and a given probability distribution.
   */
  def joint[B](that: Gen[B]): Gen[(A, B)] = map {
    x => (x, that.get)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which only contains numerical
   *  representations of its elements.
   */
  def toGenDouble(implicit d: A <:< Double): Gen[Double] = map(d(_))
}
