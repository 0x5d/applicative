package com.co.castlecorp.scala.mde.cats.applicative

import cats.Applicative

case class Person(name: String, age: Int)

object Person {

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
