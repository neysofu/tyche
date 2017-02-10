package com.github.neysofu
package tyche

import generators.MassFunction

object AutomatonFactory {

  /** Allows the automatic instantiation of an `automaton` object from the
   *  given generative function `f`. Minimum loss of information possible.
   *
   */
  def apply[A](f: () => A): Automaton[A] = f match {
    case mf: MassFunction[A] => DiscreteAutomaton(mf.mass)
    case _ => Automaton(f.apply())
  }
}
