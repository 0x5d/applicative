package com.co.castlecorp.scala.mde.cats.applicative

import cats.implicits._
import cats.Applicative
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future

class ApplicativeTest extends FunSuite with ScalaFutures with Matchers {

  test("We can lift a value into a context using pure") {
    val c = Applicative[List].pure(1)

    assert(c.head == 1)
  }

  test("With applicatives, we can apply a value in an effect to a function in an effect") {
    def concat(a: String)(b: String) = a + b

    val opt = Some("Hello")

    val mappedOpt = opt.map(concat)

    val applicative = Applicative[Option]

    val v = applicative.ap(mappedOpt)(applicative.pure("!"))

    v match {
      case Some(s) => assert(s == "Hello!")
    }

  }

  test("What if we have 2 values that we need to apply to a function?") {
    import scala.concurrent.ExecutionContext.Implicits.global

    val v1 = Future(1)
    val v2 = Future(2)

    def sum(a: Int, b: Int) = a + b

    val f = Applicative[Future].pure(sum(_, _)).ap2(v1, v2)

    whenReady(f)(_ shouldBe 3)
  }

  test("Applicatives are good. They abstract the effect of applying independent computations sequentially") {
    import com.co.castlecorp.scala.mde.cats.applicative.instances._

    val name = "David"
    val age = 24

    val person = Person.build(name, age)(stringErrorOrAp)

    person match {
      case Right(p) =>
        assert(p.name == name)
        assert(p.age == age)
    }
  }

  test("However, monads have limitations") {
    import com.co.castlecorp.scala.mde.cats.applicative.instances._

    val name = "" // Empty name
    val age = -19 // This person doesn't exist yet! D:

    val person = Person.build(name, age)(stringErrorOrAp)

    person match {
      case Left(s) => assert(s == "Name is empty") // We only get the first error! :(
    }
  }
}
