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
trait Distribution[A] { self =>

  val rnd: Random = new Random

  /** Returns a random outcome according to some probability
   *  distribution.
   */
  def get: A

  def map[B](f: A => B): Distribution[B]

  def flatMap[B](f: A => Distribution[B]): Distribution[B] = map {
    x => f(x).get
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is shrunk accordingly to a given
   *  predicate.
   */
  def given(pred: A => Boolean): Distribution[A] = map {
    x => { if (pred(x)) get else x }
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which only contains arrays of outcomes
   *  that satisfy a given predicate.
   */
  def until(pred: Seq[A] => Boolean): Distribution[Seq[A]] = map {
    x => ((ls: Seq[A]) => if (pred(ls)) ls else self.get +: ls)(Nil)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which contains only arrays of outcomes of
   *  given length.
   */
  def times(n: Int): Seq[A] = {
    Seq.fill(n)(get)
  }

  /** Returns a new bivariate probability distribution originated from both
   *  the current instance and a given probability distribution.
   */
  def joint[B](that: Distribution[B]): Distribution[(A, B)] = map {
    x => (x, that.get)
  }
}
