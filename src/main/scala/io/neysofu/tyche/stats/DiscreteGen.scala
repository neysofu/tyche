package io.neysofu.tyche
package stats

/** Represents a discrete probability distribution characterized by a
 *  probability mass function.
 */
abstract class DiscreteGen[A] extends Gen[A] with DiscreteMoments[A] {
  self =>

  /** Returns a new discrete probability distribution originated from the
   *  current instance, the outcomes of which are changed accordingly
   *  to a bijective function.
   */
  def bijectiveMap[B](f: A => B): DiscreteGen[B] = new DiscreteGen[B] {
    val mass = self.mass.map(p => (p._1, f(p._2)))
  }

  def get: A = {
    val d = random.nextDouble
    mass(cdf.indexWhere(_ > d))._2
  }
} 
  
object DiscreteGen {

  def uniform[A](vs: A*): DiscreteGen[A] = new DiscreteGen[A] {
    val mass = vs.map(x => (1.0/vs.size, x))
  }

  def Bernoulli(p: Double): DiscreteGen[Boolean] = new DiscreteGen[Boolean] {
    val mass = Seq((p, true), (1-p, false))
  }
}
