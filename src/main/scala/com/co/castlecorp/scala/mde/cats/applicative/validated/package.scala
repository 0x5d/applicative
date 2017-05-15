package com.co.castlecorp.scala.mde.cats.applicative

import cats.data.ValidatedNel

package object validated {

  type ErrorListOr[A] = ValidatedNel[ValidationError, A]
}
