package com.github.neysofu
package tyche

/** Explicit instantiation of the `Gen` trait to reduce class file size in
 *  subclasses.
 */
abstract class AbstractGen[A] extends Gen[A]

object AbstractGen {

  def apply[A](f: => A): AbstractGen[A] = new AbstractGen[A] {
    def apply: A = f
  }
}
