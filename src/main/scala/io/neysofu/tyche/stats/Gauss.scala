package io.neysofu.tyche
package stats

import scala.util.Random

case class Gauss(sd: Double, eg: Double) extends DistributionWithMoments[Double] {

  def get = Random.nextGaussian * sd + eg
}
