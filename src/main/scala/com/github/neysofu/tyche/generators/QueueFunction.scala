package com.github.neysofu
package tyche
package generators

import scala.collection.mutable.Queue

case class QueueFunction[A](refiller: () => Seq[A]) extends (() => A) {

  val queue: Queue[A] = Queue.empty[A]

  def apply: A = {
    if (queue.isEmpty) queue ++= refiller()
    queue.dequeue
  }
}
