import fpinscala.state.RNG._

int(Simple(42))
val i1:Long = 42
val rng1 = Simple(i1)
val (i2, rng2) = rng1.nextInt
val (i3, rng3) = rng1.nextInt
val (i4, rng4) = rng3.nextInt
val (i5, rng5) = nonNegativeInt(rng4)
val (d1, rng6) = double(rng5)
val ((i6, d2), rng7) = intDouble(rng6)
val ((d3, i7), rng8) = doubleInt(rng7)
val ((d4, d5, d6), rng9) = double3(rng8)
val (l, rng10) = ints(3)(rng9)
val ((i8, d7), rng11) = randIntDouble(rng10)
val ((d8, i9), rng12) = randDoubleInt(rng11)
val (s, rng13) = sequence(List(nonNegativeEven, nonNegativeEven))(rng12)
nonNegativeLessThan(219)(rng13)
