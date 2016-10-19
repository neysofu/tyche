package io.neysofu.tyche

import scala.util.Random

object Hello {

  val sample = new Distribution[Int] {
    def get = Random.nextInt(6) + 1
  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println(sample.times(4).get.toString)
    println("Done")
  }
}
