package io.neysofu.tyche

trait Distribution[A] { self =>
  
  /** Returns a random outcome according to some probability
   *  distribution.
   */
  def get: A

  override def toString = "<distribution>"

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is changed accordingly to a given
   *  function.
   */
  def map[B](f: A => B): Distribution[B] = new Distribution[B] {
    def get = f(self.get)
  }

  /** Returns a new probability distribution originated from the current
   *  instance, the sample space of which is reduced accordingly to a given
   *  predicate.
   */
  def given(pred: A => Boolean): Distribution[A] = new Distribution[A] {
    def get = { val aa = self.get; if (pred(aa)) this.get else aa }
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
