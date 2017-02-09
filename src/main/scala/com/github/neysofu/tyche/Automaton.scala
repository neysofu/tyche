package com.github.neysofu
package tyche

import scala.collection.mutable.{Seq => MutableSeq}
import scala.util.Random
import generators.{DistinctFunction, QueueFunction}

/** The `Automata` trait implements iterable sequences of infinite size.
 *
 *  {{{
 *  import com.github.neysofu.tyche.Automaton
 *
 *  object Main {
 *    val powersOfTwo: Automaton[Int] = Automaton(1, 2*_)
 *
 *  }
 *  }}}
 *
 *  Automata, similarly to iterators, are mutable data structures: all
 *  operations change their internal states. As such, one can only use an
 *  automaton after calling a method on it if its elements are independent
 *  from one another (i.e., stochastic processes, GET requests, random
 *  numbers generators). Other automata (such as the listing of the powers of
 *  2) must be discarded after any operation unless explicitly stated otherwise.
 *
 *  Automata are characterized by their respective generative function
 *  (`apply`).
 *
 *  Moments are computed through simple random sampling (
 *  [[https://goo.gl/5xB1A4 SRR]]).
 *
 *  @define hanging
 *  @note Impossible predicates may result in the hanging of the program.
 *  @tparam A the element type of this generator.
 */
trait Automaton[A] extends (() => A) {
    self =>

  /** Indicates wheter or not the automaton can evaluate other elements. */
  def isEmpty: Boolean = false

  final def tail: Automaton[A] = {
    apply
    this
  }

  /** The number of observations to include in a statistical sample.
   *
   *  The default value assures a confidence level of 95% and a margin of
   *  error of 0.01 for distributions with a normal (0.5 or less) standard
   *  deviation.
   */
  protected[this] val sampleSize: Int = 10000

  /** Finds the first index of an element of this 'automaton' that satisfy the
   *  given predicate `p`.
   *
   *  @param p the predicate used to test elements of this 'automaton'.
   *  @return  the first index that satisfy the given predicate `p`.
   */
  final def find(p: A => Boolean): Int =
    if (p(apply)) 0 else 1 + find(p)

  final def find(x: A): Int = find(_ == x)

  /** Builds a new 'automaton' by applying the given function ´f´ to each
   *  element of this 'automaton'.
   *  
   *  @tparam B the element type of the returned 'automaton'.
   *  @param  f the function to apply to each element of this 'automaton'.
   *  @return   a new 'automaton' resulting from applying the given function
   *            `f` to each element of this 'automaton' and collecting the
   *            results.
   */
  def map[B](f: A => B): Automaton[B] =
    AutomatonFactory(() => f(apply))

  def flatMap[B](f: A => Seq[B]): Automaton[B] =
    AutomatonFactory(QueueFunction(() => f(apply)))

  /** Builds a new 'automaton' by filtering out all elements of this
   *  'automaton' that don't satisfy a given condition 'p'.
   *
   *  @param p the predicate used to test elements of this 'automaton'.
   *  @return  a new 'automaton' yielding each element of this 'automaton' but
   *           the ones not satisfying the given predicate `p`. The order of
   *           the elements is preserved.
   */
  def filter(p: A => Boolean): Automaton[A] = map {
      x =>
    var a = x
    while (!p(a)) a = apply
    a
  }

  /** Builds a new 'automaton' by filtering out all elements of this
   *  'automaton' that satisfy a given condition 'p'.
   *
   *  @param p the predicate used to test elements of this 'automaton'.
   *  @return  a new 'automaton' yielding each element of this 'automaton' but
   *           the ones satisfying the given predicate `p`. The order of the
   *           elements is preserved.
   */
  final def filterNot(p: A => Boolean): Automaton[A] =
    filter (!p(_))

  /** Builds two new 'automata', the first of which yields the elements of
   *  this 'automaton' that satisfy the given predicate `p` while the second
   *  one yields the ones that don't.
   *  
   *  @param p the predicte used to test elements of this 'automaton'.
   *  @return  a pair of 'automata'. The first one yields the elements of
   *           this 'automaton' that satisfy the given predicate `p`, and the
   *           second one yields the ones that don't. The order of the
   *           elements is preserved.
   */
  final def partition(p: A => Boolean): (Automaton[A], Automaton[A]) =
    (filter(p), filterNot(p))

  def until(p: Seq[A] => Boolean) = map {
      x =>
    val seq = MutableSeq[A](x)
    while (!p(seq)) seq :+ apply
    seq
  }

  /** Builds a new 'automaton' by zipping the two 'automata' resulting from
   *  partitioning this 'automaton' with the given predicate `p` as parameter.
   *
   *  @return a new 'automaton' yielding pairs of elements of this
   *          'automaton'. The first satisfies p, the other doesn't.
   */
  final def opposites(p: A => Boolean): Automaton[(A, A)] = {
    val (a, b) = partition(p)
    a zip b
  }

  def fold[B](start: B, f: (B, A) => B): Automaton[B] = {
    var lastval = start
    map {
        x =>
      lastval = f(lastval, x)
      lastval
    }
  }

  def distinct: Automaton[A] =
    AutomatonFactory(DistinctFunction(() => this.apply()))

  /** Zips together the elements set of this process with another one's.
   *
   *  @tparam B the element type of the given process `that`.
   *  @param that the process zipped with this process.
   *  @return a new process with a new elements set resulting from zipping
   *  the elements set of this process with another one's.
   */
  def zip[B](that: Automaton[B]): Automaton[(A, B)] = map ((_, that()))

  /** Fills a collection with elements of this process.
   *
   *  @param n the length of the returned collection.
   *  @return a `n`-sized collection filled with elements of this process.
   *  The order of the elements is preserved.
   */
  final def take(n: Int): Seq[A] = Seq.fill(n)(apply)


  /** Builds a new 'automaton' by joining together the elements of this
   *  'automaton' in collections of fixed size `n`.
   *
   *  @param n the size of the yielded collections
   *  @return  a new 'automaton' yielding collections of size `n` containing
   *           elements of this 'automaton'. The order of the elements is
   *           preserved.
   */
  final def grouped(n: Int): Automaton[Seq[A]] = map (_ +: take(n-1))

  /** Creates a statistical sample.
   *
   *  @return a `sampleSize`-sized collection filled with elements of this
   *  process. The order of the elements is preserved.
   */
  final def sample: Seq[A] = take(sampleSize)

  def exists(p: A => Boolean): Boolean = sample exists p

  /** Computes the estimated probability that an element of this process will
   *  satisfy a predicate.
   *
   *  @param p the predicate used to test observations.
   *  @return the ratio of the number of observations which satisfy the given
   *  predicate `p` to the sample size of this process.
   */
  def probabilityOf(p: A => Boolean): Double =
    sample.count(p).toDouble / sampleSize
  
  /** Computes the estimated [[https://goo.gl/LruXGw expected value]] (mean)
   *  of this process.
   *
   *  @return the arithmetic mean of a statistical sample of this process.
   */
  def mean(implicit toDouble: A => Double): Double =
    (sample map(toDouble(_) / sampleSize)).sum
  
  /** Computes the estimated [[https://goo.gl/QrSlFY standard deviation]] of
   *  this process.
   *
   *  @return the standard deviation of a statistical sample of this process.
   */
  def stdDeviation(implicit toDouble: A => Double): Double = {
    val m = mean
    (sample map (x => (x*x - m) / sampleSize)).sum
  }
  
  /** Computes the estimated [[https://goo.gl/Wzlr6p variance]] of this
   *  process.
   *
   *  @return the square of the estimated standard deviation of this process.
   */
  final def variance(implicit toDouble: A => Double): Double =
    Math.pow(stdDeviation, 2)

  /** Converts this process to a stream.
   *
   *  @return an infinitely long stream of the elements of this process.
   */
  final def toStream: Stream[A] = apply #:: toStream
}

object Automaton {

  def apply[A](f: => A): Automaton[A] = new Automaton[A] {
    def apply: A = f
  }

  def fromStream[A](stream: Stream[A]): Automaton[A] = {
    val iter = stream.iterator
    Automaton(iter.next)
  }

  def empty: Automaton[Unit] = Automaton(())
}
