import fpinscala.errorhandling._

import Option._

//val F: Applicative[Option] = ...
case class Employee(name: String, id: Int)
case class Pay(rate: Double, hoursPerYear: Double)

object AnnualPay {
  def format(e: Option[Employee], pay: Option[Pay]): Option[String] =
    map2(e, pay) { (e, pay) =>
      s"${e.name} makes ${pay.rate * pay.hoursPerYear}"
    }
}

object AnnualPay_Refactored {
  def format(name: Option[String], pay: Option[Double]): Option[String] =
    map2(e, pay) { (e, pay) => s"$e makes $pay" }
}

val e: Option[Employee] = Some(Employee("Bruce Leewees", 666))
val pay: Option[Pay] = Some(Pay(20, 1000))

AnnualPay.format(e, pay)

AnnualPay_Refactored.format(
  e.map(_.name),
  pay.map(pay => pay.rate * pay.hoursPerYear))

def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) =
  (i, i2) => (f(i), g(i2))
