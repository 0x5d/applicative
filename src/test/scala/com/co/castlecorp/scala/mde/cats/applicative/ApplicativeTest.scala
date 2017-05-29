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

  case class Person(name: String, age: Int)

  object Person {

    type StringErrorOr[R] = Either[String, R]

    def build(name: String, age: Int)(implicit a: Applicative[StringErrorOr]) = {
      a.ap2(a.pure( (a: String, n: Int) => Person(a, n)))(checkName(name), checkAge(age))
    }

    private def checkName(n: String): StringErrorOr[String] = {
      if (n.isEmpty) Left("Name is empty")
      else Right(n)
    }

    private def checkAge(a: Int): StringErrorOr[Int] = {
      if (a < 0) Left("No embryos allowed")
      else if (a > 110) Left("Go away, gramps")
      else Right(a)
    }
  }

  test("Applicatives are good. They abstract the effect of applying independent computations sequentially") {

    val name = "David"
    val age = 24

    val person = Person.build(name, age)

    person match {
      case Right(p) =>
        assert(p.name == name)
        assert(p.age == age)
    }
  }

  test("However, monads have limitations") {

    val name = "" // Empty name
    val age = -19 // This person doesn't exist yet! D:

    val person = Person.build(name, age)

    person match {
      case Left(s) => assert(s == "Name is empty") // We only get the first error! :(
    }
  }
}
