import java.util.concurrent.{Executors, ExecutorService}

import fpinscala.parallelism._
import fpinscala.testing._

val smallInt = Gen.choose(-10,10)

import Prop._

val maxProp = forAll(listOf1(smallInt)) {
  l => val max = l.max; !l.exists(_ > max)
}

run(maxProp)
val sortedProp = forAll(listOf1(smallInt)) {
  l =>
    val sorted = l.sorted
    sorted.foldLeft((sorted.head, true)) { case ((h, b), a) => (a, b && (a >= h)) }._2
}

run(sortedProp)
Gen.unit(1).map(_ + 1) == Gen.unit(2)
val ES: ExecutorService = Executors.newCachedThreadPool
val p1 = Prop.forAll(Gen.unit(Par.unit(1)))( i =>
  Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

val p2 = Gen.check {
  val p = Par.map(Par.unit(1))(_ + 1)
  val p2 = Par.unit(2)
  p(ES).get == p2(ES).get
}

