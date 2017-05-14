package com.co.castlecorp.scala.mde.cats

import cats.implicits._
import cats.Applicative

package object applicative {

  type StringErrorOr[R] = Either[String, R]

  object instances {

    implicit val stringErrorOrAp = new Applicative[StringErrorOr] {

      override def pure[A](x: A): StringErrorOr[A] = Either.right[String, A](x)

      override def ap[A, B](ff: StringErrorOr[(A) => B])(fa: StringErrorOr[A]): StringErrorOr[B] = for {
        f <- ff
        a <- fa
      } yield f(a)
    }
  }
}
