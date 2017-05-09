package com.co.castlecorp.scala.mde.cats.applicative

import cats.implicits._
import cats.Applicative
import org.scalatest.FunSuite

class ApplicativeTest extends FunSuite {

  test("We can lift a value into a context using pure") {
    val c = Applicative[List].pure(1)

    assert(c.head == 1)
  }
}
