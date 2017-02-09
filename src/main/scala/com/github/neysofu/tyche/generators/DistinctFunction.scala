package com.github.neysofu
package tyche
package generators

import scala.collection.mutable.MutableList


case class DistinctFunction[A](f: () => A) extends (() => A) {

  val memo: MutableList[A] = MutableList.empty[A]

  def apply: A = {
    val a = f.apply
    if (memo contains a) {
      apply
    } else {
      memo += a
      a
    }
  }
}
