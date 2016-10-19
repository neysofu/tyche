package io.neysofu.tyche.main

import io.neysofu.tyche.stats.Distribution
import scala.util.Random

object Hello {

  val sample = new Distribution[Double] {
    def get = Random.nextInt(6) + 1
  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    println(sample.times(4).get.toString)
    println(sample.variance)
    println("Done")
  }
}
