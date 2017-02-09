package com.github.neysofu
package tyche

import generators.MassFunction

object AutomatonFactory {

  def apply[A](f: () => A): Automaton[A] = f match {
    case mf: MassFunction[A] => DiscreteAutomaton(mf.mass)
    case _ => Automaton(f.apply())
  }
}
