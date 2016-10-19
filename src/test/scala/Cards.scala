package io.neysofu.tyche

object Cards {

  sealed trait Card
  case object C extends Card
  case object D extends Card
  case object H extends Card
  case object S extends Card
}
