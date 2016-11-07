package io.neysofu.tyche

trait MarkovGen[A] extends Gen[MarkovGen[A]] { self =>

  val state: A

  val rule: A => Map[A, Double]

  def get = new MarkovGen[A] {
    val state = new DiscreteDistribution(self.rule(self.state)).get
    val rule = self.rule
  }

  def getAfter(n: Int): MarkovGen[A] = take(n).last
}
