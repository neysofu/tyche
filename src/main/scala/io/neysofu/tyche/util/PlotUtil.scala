package io.neysofu.tyche
package util

object PlotUtil {

  def frameString(str: String): String = {
    val nl = "\n"
    val lineLength = str.indexOf(nl)
    val headLine = "╔" + "═" * lineLength + "╗"
    val lastLine = "╚" + "═" * lineLength + "╝"
    headLine + nl + str.split(nl).map { line =>
      "║" + line + "║" + nl
    }.mkString + lastLine
  }
}
