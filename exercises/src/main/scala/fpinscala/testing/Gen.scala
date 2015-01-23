package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, n, rng) => or(run(max, n, rng), p.run(max, n, rng))}

  def ||(p: Prop): Prop = Prop { (max, n, rng) => and(run(max, n, rng), p.run(max, n, rng))}
}

object Prop {

  type MaxSize = Int
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen((n: Int) => Gen.listOfN(n, g))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => Gen.forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduceLeft(_ && _)
      prop.run(max, n, rng)
  }

  def or[A](o1: Option[A], o2: Option[A]): Option[A] = o1 match {
    case None => o2
    case Some(a) => Some(a)
  }

  def and[A](o1: Option[A], o2: Option[A]): Option[A] = o1 match {
    case None => None
    case Some(a) => o2
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def int: State[RNG, Int] = State(_.nextInt)

  def positiveInt: State[RNG, Int] =
    int.flatMap(i => if (i == Integer.MIN_VALUE) positiveInt else State.unit(i))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(positiveInt.map(i => start + i % (stopExclusive - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    if (n <= 0)
      Gen(State.unit(List[A]()))
    else {
      val lg = listOfN(n - 1, g)
      Gen(g.sample.map2(lg.sample)(_ :: _))
    }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) None else Some((a.toString, i))
      } catch {
        case e: Exception => Some((buildMsg(a, e), i))
      }
    }.find(_.isDefined).getOrElse(None)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}

case class Gen[A](sample: State[RNG,A]) {
  def map[A, B](f: A => B): Gen[B] = ???

  def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
}

case class SGen[A](forSize: Int => Gen[A])
