package fpinscala.applicative

import java.util.Date

case class WebForm(name: String, date: Date, phone: String)

object WebFormValidation {
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case e: Exception =>
        Failure("Birthdate must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] =

    Applicative.validationApplicative.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
        WebForm(_, _, _))
}
