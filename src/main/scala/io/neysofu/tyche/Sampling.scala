package io.neysofu.tyche

/** Provides a default sample size for sampling purposes.
  */
trait Sampling {

  /** The number of observations in include in statistical samples.
    *
    * Visit
    *  [[http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Power/BS704_Power3.html this]]
    * webpage for sample size determination tips when dealing with continuous
    * random variables.
    *
    * The default value assures a confidence level of 95% and a margin of
    * error of 0.01 for distributions with a normal (0.5 or less) standard
    * deviation.
    */
  val sampleSize: Int = 10000
  val nthSampleSize: Double = 1.0 / sampleSize
}
