package io.neysofu.tyche
package stats

import scala.util.Random

case class Gauss(sd: Double = 1, eg: Double = 0) extends DistributionWithMoments[Double] {

  def get = Random.nextGaussian * sd + eg
}
