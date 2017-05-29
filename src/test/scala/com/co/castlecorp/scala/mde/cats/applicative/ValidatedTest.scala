package com.co.castlecorp.scala.mde.cats.applicative

import cats.implicits._
import cats.data.{Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.FunSuite

class ValidatedTest extends FunSuite {

  test("Validated is an applicative functor, but not a monad!") {
    val v = Valid(312)
    val i = Invalid("Beep boop: error")

    /*
    * That means we can't make for-comprehensions.
    * It has a 'map' combinator, but no 'flatMap'
    * */
  }

  case class ClientRegistration(name: String, age: Int, address: String)

  object ClientRegistration {

    type ErrorListOr[A] = ValidatedNel[ValidationError, A]

    sealed trait DomainError {

      val message: String
    }
    final case class ValidationError(override val message: String) extends DomainError


    def validate(name: String, age: Int, address: String): ValidatedNel[ValidationError, ClientRegistration] = {
      (
        validateName(name) |@|
        validateAge(age) |@|
        validateAddress(address)
      ).map(ClientRegistration.apply(_, _, _))
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

  test("It's analogous to Either (but remember, it's not a monad)") {

    val name = "Josefa"
    val age = 80
    val address = "Evergreen St #178"

    val r = ClientRegistration.validate(name, age, address)

    r match {
      case Valid(ClientRegistration(n, ag, ad)) =>
        assert(n == name)
        assert(ag == age)
        assert(ad == address)
    }
  }

  test("But the best thing is, Validated allows us to accumulate errors") {

    val name = "Josephine"
    val age = 180
    val address = "asd"

    val r = ClientRegistration.validate(name, age, address)

    r match {
      case Invalid(errs) =>
        val errList = errs.toList
        assert(errList.length == 2)
        assert(errList.head.message == "Go away, gramps")
        assert(errList.tail.head.message == "Impossibly short address")
    }
  }
}
