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

  def approxBinomial(n: Int, p: Double): Gen[Double] =
    Gen(Gauss(n*p * (1-p), n*p).apply)
}
