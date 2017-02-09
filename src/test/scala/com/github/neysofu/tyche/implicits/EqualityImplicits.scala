package com.github.neysofu
package tyche
package implicits

import org.scalactic.TolerantNumerics

object EqualityImplicits {

  val epsilon = 1e-5f
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)
}
