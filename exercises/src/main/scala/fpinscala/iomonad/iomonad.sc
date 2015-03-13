import fpinscala.iomonad.IO2a._

val _f = (x: Int) => x

val _g = List.fill(100000)(_f).foldLeft(_f)(_ compose _)

//g(42)

val f: Int => IO[Int]  = (x: Int) => Return(x)

val g = List.fill(100000)(f).foldLeft(f) {
//  (a, b) => x => Suspend(() => a(x).flatMap(b))
//  case _ => (x: Int) => Suspend(() => x)
  (a, b) => x => Suspend(() => ()).flatMap { _ => a(x).flatMap(b) }
}

val x1 = run(g(42))
