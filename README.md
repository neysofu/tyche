# Tyche ![build](https://travis-ci.org/neysofu/tyche.svg?branch=master)
Tyche is a robust, fully modular *JVM* library written in Scala for
numerical analysis over probability distributions and other stochastic
processes.

    sealed trait Child
	case object M extends Child
	case object F extends Child

	def family = DiscreteDistribution.uniform(M, F).until(_.contains(M))

	val sampleSize = 10000
	// The expected number of children per family
	1.0 * family.map(_.size).times(sampleSize).sum / sampleSize
