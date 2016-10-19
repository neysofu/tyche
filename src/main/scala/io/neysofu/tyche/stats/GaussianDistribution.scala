package io.neysofu.tyche.stats

import io.neysofu.tyche.stats.Distribution
import scala.util.Random

abstract class GaussianDistribution(stdev: Double, expval: Double)
    extends Distribution[Double] {

  def get: Double = Random.nextGaussian() * stdev + expval
}
