package com.co.castlecorp.scala.mde.cats.applicative.validated

sealed trait DomainError {

  val message: String
}
final case class ValidationError(override val message: String) extends DomainError
