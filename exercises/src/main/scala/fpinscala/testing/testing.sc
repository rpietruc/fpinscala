import fpinscala.testing._

val smallInt = Gen.choose(-10,10)

import Prop._

val maxProp = forAll(listOf(smallInt)) {
  l => val max = l.max; !l.exists(_ > max)
}

run(maxProp)
