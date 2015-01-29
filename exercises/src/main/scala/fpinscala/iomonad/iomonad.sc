import fpinscala.iomonad.IO2a._

val _f = (x: Int) => x

val _g = List.fill(100000)(_f).foldLeft(_f)(_ compose _)

//g(42)

val f: Int => IO[Int]  = (x: Int) => Return(x)

val g = List.fill(100000)(f).foldLeft(f) {
  case (f, z) => (x: Int) => Suspend(() => x)
}

val x1 = run(g(42))
