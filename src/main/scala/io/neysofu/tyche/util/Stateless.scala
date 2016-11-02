package io.neysofu.tyche
package util

class Cache[A](var state: A) { self =>
  
  def is[B](f: A => B): Unit = {
    var state: B = f(self.state)
  }
}
