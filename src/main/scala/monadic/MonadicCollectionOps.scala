package monadic

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
class MonadicCollectionOps(implicit ec: ExecutionContext) {

  def optionModifier(
      value: Option[Int],
      modifier: Int => Int):
      Option[Int] = value match{
      case Some(v) => Option(modifier(v))
      case None => None
    }


  def optionModifierOption(
      value: Option[Int],
      modifier: Int => Option[Int]
  ): Option[Int] = value match{
    case Some(v) => modifier(v)
    case None => None
  }

  def tryFunction(
      value: Int,
      divisor: Int,
      function: (Int, Int) => Double
  ): Try[Double] = Try(function(value, divisor))


  def tryModifierExceptionHandled( // neveikia
      value: Int,
      modifier: Int => Int
  ): Try[Double] = Try(modifier(value)) match {
    case Success(x) => Try(modifier(x).toDouble*2)
    case Failure(f) => f match {
      case _:TooSmall => Try(0.0)
      case exception:SixNotAllowed =>
        Try(throw new RuntimeException("Error while applying modifier", exception))
      case _:TooLarge => Try(modifier(value).toDouble)
    }

  }

  def eitherModifierExceptionHandled(
      value: Int,
      modifier: Int => Int
  ): Either[ModifierFailure, Double] =
    try{
    Right(modifier(value)*2)
  } catch {
    case _:TooSmall => Right(0.0)
    case exception:SixNotAllowed =>
      Left(ModifierFailure(new RuntimeException("Error while applying modifier", exception)))
    case exception:TooLarge =>Left(ModifierFailure(exception))
  }

  def multiplesFuture(
      intSeqGenerator: (Int, Int) => Future[Seq[Int]],
      isMultipleOf: Int => Int => Future[Boolean]
  )(
      from: Int,
      to: Int,
      multipleOf: Int
  ): Future[Either[MultiplesSearchFailure, Seq[Int]]] = ???
}
