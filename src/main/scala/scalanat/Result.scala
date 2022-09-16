package scalanat

/**
 * An extended option that can return either an object, or a message.
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
                case (Failure(m), _) => Failure(m)
                case (_, Failure(m)) => Failure(m)
            }
        s.foldLeft(Success(Nil))(f) match {
            case Success(ts) => Success(ts.reverse)
            case Failure(m) => Failure(m)
        }


case class Success[T](t: T) extends Result[T]
case class Failure[T](msg: String) extends Result[T]
