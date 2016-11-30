package com.github.neysofu
package tyche
package algorithms

/** Builds a new discrete uniform distribution.
 *  
 *  @param values the sample space.
 *  @return a discrete uniform distribution with the given sample space
 *  `values`.
 */ 
case class DiscreteUniform[A](space: A*) extends DiscreteGen[A] {

  def mass: Map[A, Double] = {
    require(!space.isEmpty, "The sample space cannot be empty.")
    val w = 1.0 / space.distinct.size
    space.map(x => x->w).toMap
  }
}
