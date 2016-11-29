package com.github.neysofu
package tyche

object BernoulliProcess {

  sealed trait Outcome
  object Outcome1 extends Outcome
  object Outcome0 extends Outcome
}
