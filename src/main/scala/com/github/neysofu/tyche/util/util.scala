package com.github.neysofu.tyche

import scala.collection.mutable

/** A collectcom.github. of common utilities.
 */
package object util {

  def square(x: Double): Double = Math.pow(x, 2)

  def buildTable[A](from: Seq[A]): Map[A, Map[A, Int]] = {
    var table = mutable.Map[A, mutable.Map[A, Int]]()
    for {
      pair <- from.sliding(2)
      a = pair(0)
      b = pair(1)
    } {
      if (! table.contains(a))
        table(a) = mutable.Map(b -> 1)
      else if (! table(a).contains(b))
        table(a)(b) = 1
      else
        table(a)(b) += 1
    }
    table.toMap.map(kv => kv._1 -> kv._2.toMap)
  }

  def map2function[A, B](map: Map[A, B]): A => B = a => map(a)
}
