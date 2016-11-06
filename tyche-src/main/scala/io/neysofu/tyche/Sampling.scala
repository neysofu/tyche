package io.neysofu.tyche

/** This trait provides a default sample size for sampling purposes.
 */
trait Sampling {

  /** The number of observations to include in a statistical sample.
   *
   * The default value assures a confidence level of 95% and a margin of
   * error of 0.01 for distributions with a normal (0.5 or less) standard
   * deviation.
   */
  val sampleSize: Int = 10000
  protected lazy val nthSampleSize: Double = 1.0 / sampleSize
}
