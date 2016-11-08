package com.github.neysofu.tyche

/** Provides a default sample size for sampling purposes.
 */
trait Sampling {

  /** The number of observatcom.github.s to include in a statistical sample.
   *
   *  The default value assures a confidence level of 95% and a margin of
   *  error of 0.01 for distributcom.github.s with a normal (0.5 or less) standard
   *  deviatcom.github..
   */
  val sampleSize: Int = 10000
}
