package com.co.castlecorp.scala.mde.cats.applicative

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.co.castlecorp.scala.mde.cats.applicative.validated.ClientRegistration
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
