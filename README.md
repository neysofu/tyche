# Tyche ![build](https://travis-ci.org/neysofu/tyche.svg?branch=master) ![scala version](https://img.shields.io/badge/scala-2.12.0-blue.svg) ![doc](https://img.shields.io/badge/doc-neysofu.github.io%2Ftyche-brightgreen.svg)
Tyche is a simple yet robust statistical library for the JVM. Many JVM-hosted
numerical libraries offer similar functionalities but they fail to offer
modularity. Tyche is built from the ground up to first be able to perform
common data manipulation techniques.

Behold, the power of Tyche:

	import io.neysofu.tyche

	val sampleSize = 10000

	// How many times do I have to toss a coin before a head comes up?
	sealed trait Coin
	object Head extends Coin
	object Tail extends Coin

	val toss = DiscreteDistribution.uniform(Head, Tail)
	  .until(_ contains Head)
	  .take(sampleSize)
	  .flatten.size / sampleSize
