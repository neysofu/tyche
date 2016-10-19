package io.neysofu.tyche.stats

trait Distribution[A] { self =>

  /** The default values for deciding the number of trial runs. See
   *    http://en.wikipedia.org/wiki/Checking_whether_a_coin_is_fair
   */
  val stdDev: Double = 2
  val maxError: Double = 0.01

  /** Returns a random outcome according to some probability
   *  distribution.
   */
  def get(): A

  override def toString = "<distribution>"

  /** Returns the expected value (mean) of the probability distribution
   *  accordingly to an estimator of true probability.
   */
  def mean(implicit toDouble: A <:< Double): Double = {
    val n = (stdDev, maxError) match {
      case (2, 0.01) => 10000
      case _ => (Math.pow(stdDev, 2) / (4 * Math.pow(maxError, 2))).toInt
    }
    times(n).get.map(toDouble(_)).sum / n
  }

  /** Returns the variance of the probability distribution.
   */
  def variance(implicit toDouble: A <:< Double): Double = {
    val avg = mean
    map(x => Math.pow(toDouble(x) - avg, 2)).mean
  }

  /** Returns the standard deviation of the probability function.
   */
  def standardDeviation(implicit toDouble: A <:< Double): Double = {
    Math.sqrt(variance)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is changed accordingly to a given
   *  function.
   */
  def map[B](f: A => B): Distribution[B] = new Distribution[B] {
    def get = f(self.get)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is shrunk accordingly to a given
   *  predicate.
   */
  def given(pred: A => Boolean): Distribution[A] = new Distribution[A] {
    def get = { val aa = self.get; if (pred(aa)) self.get else aa }
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which only contains arrays of outcomes
   *  that satisfy a given predicate.
   */
  def until(pred: Seq[A] => Boolean): Distribution[Seq[A]] =
    new Distribution[Seq[A]] {
      def get = ((ls: Seq[A]) => if (pred(ls)) ls else self.get +: ls)(Nil)
    }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which contains only arrays of outcomes of
   *  given length.
   */
  def times(n: Int): Distribution[Seq[A]] = new Distribution[Seq[A]] {
    def get = Seq.fill(n)(self.get)
  }

  /** Returns a new bivariate probability distribution originated from both
   *  the current instance and a given probability distribution.
   */
  def joint[B](that: Distribution[B]): Distribution[(A, B)] = 
    new Distribution[(A, B)] {
      def get = (self.get, that.get)
    }
}
