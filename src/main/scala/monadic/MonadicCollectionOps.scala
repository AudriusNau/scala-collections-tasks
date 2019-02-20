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
  ): Option[Int] = ???




  def tryFunction(
      value: Int,
      divisor: Int,
      function: (Int, Int) => Double
  ): Try[Double] = Try(function(value, divisor))


  def tryModifierExceptionHandled( // neveikia
      value: Int,
      modifier: Int => Int
  ): Try[Double] = Try(modifier(value)) match {
    case Success(x) => Success(x)
    case Failure(exception) => exception match{
      case e: TooSmall => Try(0)
      case e:SixNotAllowed => Try(10)
      case e: TooLarge=> Try(20)
    }
  }

  def eitherModifierExceptionHandled(
      value: Int,
      modifier: Int => Int
  ): Either[ModifierFailure, Double] = ???

  def multiplesFuture(
      intSeqGenerator: (Int, Int) => Future[Seq[Int]],
      isMultipleOf: Int => Int => Future[Boolean]
  )(
      from: Int,
      to: Int,
      multipleOf: Int
  ): Future[Either[MultiplesSearchFailure, Seq[Int]]] = ???
}
