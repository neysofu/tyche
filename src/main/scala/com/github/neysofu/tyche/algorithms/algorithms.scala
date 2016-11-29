package com.github.neysofu
package tyche

import scala.util.Random
import scala.collection.mutable.{Map => Dict}
import util.Memo

package object algorithms {

  /** Creates a chi‐squared distribution.
   *
   *  @param k the ''𝑘'' parameter of the returned distribution.
   *  @return a chi‐squared distribution ''χ²(𝑘)''.
   */
  def chiSquare(k: Int): Gen[Double] = Gen {
    var sum: Double = 0
    val standardNormal = Gauss(0, 1)
    for (n <- 1 to k)
      sum += Math.pow(standardNormal(), 2)
    sum
  }

  /** Creates a uniform distribution.
   *
   *  @return a uniform distribution ''unif(0, 1)''.
   */
  def uniform: Gen[Double] = Gen(Random.nextDouble)

  /** Builds a new discrete uniform distribution.
   *  
   *  @param values the sample space.
   *  @return a discrete uniform distribution with the given sample space
   *  `values`.
   */
  def uniform[A](values: A*): DiscreteDistribution[A] = {
    require(!values.isEmpty, "The sample space cannot be empty.")
    val w = 1.0 / values.distinct.size
    DiscreteDistribution(values.map(x => x->w).toMap)
  }

  def Bernoulli(p: Double): DiscreteDistribution[Boolean] =
    DiscreteDistribution(Map(true -> p, false -> (1-p)))

  /** Returns a binomial distribution.
   *
   *  @param n the number of independent yes/no experiments
   *  @param p the success probability
   *  @throws java.lang.IllegalArgumentException if `n` is too large.
   */
  def binomial(n: Int, p: Double): DiscreteDistribution[Int] = {
    // TODO: define gen by Seq.fill(n)(get) and making the mass lazy
    val mass = Dict.empty[Int, Double]
    lazy val binCoefficient: Memo[(Int, Int), Long] = Memo {
      case (n, k) => if (k == 0 || k == n) {
        1L
      } else if (k > n/2) {
        binCoefficient(n, n-k)
      } else {
        binCoefficient(n-1, k-1) + binCoefficient(n-1, k)
      }
    }
    for (k <- 0 to n) {
      val pk = Math.pow(p, k)
      val qj = Math.pow(1-p, n-k)
      mass += (k -> binCoefficient(n, k) * pk * qj)
    }
    DiscreteDistribution(mass.toMap)
  }
  
  def approxBinomial(n: Int, p: Double): Gen[Double] =
    Gen(Gauss(n*p * (1-p), n*p).apply)
}
