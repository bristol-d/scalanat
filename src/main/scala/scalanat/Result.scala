package scalanat

/**
 * An extended option that can return either an object, or a message.
 */
sealed trait Result[T]

case class Success[T](t: T) extends Result[T]
case class Failure[T](msg: String) extends Result[T]
