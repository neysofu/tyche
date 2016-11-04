# Tyche ![build](https://travis-ci.org/neysofu/tyche.svg?branch=master)
Tyche is a simple yet robust statistical library for the JVM. Many JVM-hosted
numerical libraries offer similar functionalities but they fail to offer
modularity. Tyche is built from the ground up to first be able to perform
common data manipulation techniques.

Behold, the power of Tyche:

	import io.neysofu.tyche

	// How many times do I have to toss a coin before a head comes up?
	sealed trait Coin
	object Head extends Coin
	object Tail extends Coin

	val toss = DiscreteDistribution
	  .uniform(Head, Tail)
	  .until()

numerical analysis over probability distributions and other stochastic
processes.

    sealed trait Child
	case object M extends Child
	case object F extends Child

	def family = DiscreteDistribution.uniform(M, F).until(_.contains(M))

	val sampleSize = 10000
	// The expected number of children per family
	1.0 * family.map(_.size).times(sampleSize).sum / sampleSize
