package io.neysofu.tyche
package stats

import scala.util.Random

/** Represents a generic probability distribution. It supports sampling,
 *  plotting, data manipulation features, zipping and more.
 *
 *  Resources
 *  ---------
 *  1. B.Gnedenko - Kurs teorii verojatnostej
 *  2. D.E.Knuth - Seminumerical algorithms
 */
trait Gen[A] { self =>

  val rnd: Random = new Random

  /** Returns a random outcome according to some probability
   *  distribution.
   */
  def get: A

  /** Returns a new probability distribution originated from the current
   *  instance, the outcomes of which are changed accordingly to a given
   *  function.
   */
  def map[B](f: A => B): Gen[B] = new Gen[B] {
    def get = f(self.get)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is shrunk accordingly to a given
   *  predicate.
   */
  def given(pred: A => Boolean): Gen[A] = new Gen[A] {
    def get: A = {
      val g = self.get
      if (pred(g)) g else get
    }
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which only contains arrays of outcomes
   *  that satisfy a given predicate.
   */
  def until(pred: Seq[A] => Boolean): Gen[Seq[A]] = map { x =>
    def growSeq(ls: Seq[A]): Seq[A] = {
      if (pred(ls))
        ls
      else
        growSeq(ls :+ get)
    }
    growSeq(Seq(x))
  }

  /** Returns an array filled with outcomes of the desired length.
   */
  def times(n: Int): Seq[A] = Seq.fill(n)(get)

  /** Returns a new bivariate probability distribution originated from both
   *  the current instance and a given probability distribution.
   */
  def joint[B](that: Gen[B]): Gen[(A, B)] = map {
    x => (x, that.get)
  }
}
