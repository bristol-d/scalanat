package scalanat

/**
 * An extended option that can return either an object, or a message and an
 * optional context object that describes the steps that succeeded.
 */
sealed trait Result[T]

object Result:
    /**
     * If all elements are success, return a success with the elements extracted.
     * If any failures exist, return a failure with the first one.
     */
    def applySeq[T](s: Seq[Result[T]]): Result[Seq[T]] =
        def f(acc: Result[Seq[T]], item: Result[T]): Result[Seq[T]] =
            (acc, item) match {
                case (Success(ts), Success(t)) => Success (t +: ts)
                case (Failure(m, c), _) => Failure(m, c)
                // Help, we've build a monad by accident.
                case (_, Failure(m, c)) => Failure(m, c match {
                    case None => None
                    case Some(t) => Some(Seq(t))
                })
            }
        s.foldLeft(Success(Nil))(f) match {
            case Success(ts) => Success(ts.reverse)
            case Failure(m, c) => Failure(m, c)
        }


case class Success[T](t: T) extends Result[T]
case class Failure[T](msg: String, context: Option[T]) extends Result[T]:
    def this(msg: String) = this(msg, None)
