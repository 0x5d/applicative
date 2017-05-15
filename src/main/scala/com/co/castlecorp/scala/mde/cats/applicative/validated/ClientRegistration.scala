package com.co.castlecorp.scala.mde.cats.applicative.validated

import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNel}
import cats.syntax.cartesian._

case class ClientRegistration(name: String, age: Int, address: String)

object ClientRegistration {

  def validate(name: String, age: Int, address: String): ValidatedNel[ValidationError, ClientRegistration] = {
    (
      validateName(name) |@|
      validateAge(age) |@|
      validateAddress(address)
    ).map(ClientRegistration.apply)
  }

  private def validateName(n: String): ErrorListOr[String] = {
    if (n.isEmpty) Validated.invalidNel(ValidationError("Empty name"))
    else Valid(n)
  }

  private def validateAge(a: Int): ErrorListOr[Int] = {
    if (a < 0) Validated.invalidNel(ValidationError("No embryos allowed"))
    else if (a > 110) Validated.invalidNel(ValidationError("Go away, gramps"))
    else Valid(a)
  }

  private def validateAddress(a: String): ErrorListOr[String] = {
    if (a.isEmpty) Validated.invalidNel(ValidationError("Empty address"))
    else if (a.length < 5) Validated.invalidNel(ValidationError("Impossibly short address"))
    else Valid(a)
  }
}