# Tyche
Tyche is a robust, fully modular *JVM* library written in Scala for
numerical analysis over probability distributions and other stochastic
processes.

    sealed trait Child
	case object M extends Child
	case object F extends Child

	def family = Commons.newDiscreteUniform(Seq(M,F)).until(_.contains(M))

	val sampleSize = 10000
	1.0 * family.times(sampleSize).flatten.count(_ == F) / sampleSize
