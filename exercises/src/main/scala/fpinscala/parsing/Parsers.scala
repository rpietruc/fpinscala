package fpinscala.parsing

import java.util.regex._
import fpinscala.datastructures._
import fpinscala.errorhandling._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] {

  // so inner classes may call methods of trait
  // or: val self = this
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(a).map(b => f(b)))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
  //    map(product(p1, p2))(f.tupled)
    for {a <- p1; b <- p2} yield f(a, b) // p1.flatMap(a => p2.map(b => f(a, b)))

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def many[A](p: Parser[A]): Parser[List[A]] =
    or(map2(p, many(p))(Cons(_, _)), succeed(List()))

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {a <- p; b <- p2} yield (a, b) // p1.flatMap(a => p2.map(b => (a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    product(p, many(p)).map { case (a, l) => Cons(a, l)}

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0)
      succeed(List())
    else
      map2(p, listOfN(n - 1, p)) { case (a, l) => Cons(a, l)}

  def slice[A](p: Parser[A]): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def slice: Parser[String] = self.slice(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    def many[B >: A]: Parser[List[B]] = self.many(p)

    def many1[B >: A]: Parser[List[B]] = self.many1(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, map(p)(a => a))(in)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = if (line > 1) input.slice(0, offset + 1).reverse.indexOf('\n') else offset

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}
