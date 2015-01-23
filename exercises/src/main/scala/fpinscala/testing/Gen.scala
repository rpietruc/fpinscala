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

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def int: State[RNG, Int] = State(_.nextInt)

  def positiveInt: State[RNG, Int] =
    int.flatMap(i => if (i == Integer.MIN_VALUE) positiveInt else State.unit(i))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(positiveInt.map(i => start + i%(stopExclusive - start)))
}
case class Gen[A](sample: State[RNG,A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}


trait SGen[+A] {

}

