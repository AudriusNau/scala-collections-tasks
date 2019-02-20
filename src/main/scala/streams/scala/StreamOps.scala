package streams.scala

import java.time.{LocalDate, LocalTime}

/**
  * Refer to [[scala.collection.immutable.Stream]]
  */
class StreamOps {

  def intStream(
      from: Int,
      takeTotal: Int
  ): Stream[Int] = {
    val stream: Stream[Int] = Stream.from(from)
    stream.take(takeTotal)
  }
  def intMultStream(
      from: Int,
      takeTotal: Int,
      multiplier: Int => Int
  ): Stream[Int] = {
    val stream: Stream[Int] = Stream.from(from)
    stream.take(takeTotal).map(multiplier)
  }

  def intStreamMedian(
      from: Int,
      takeTotal: Int
  ): Double = {
    val stream: Stream[Int] = Stream.from(from)

    val mid = (takeTotal % 2)
     mid match{
      case 1 => stream(takeTotal/2)
      case 0 => (stream(takeTotal/2)+stream(takeTotal/2-1)).toDouble/2
    }
  }

  def datesStream(
      from: LocalDate,
      to: LocalDate
  ): Stream[LocalDate] = {
    if( from.isAfter(to)) Stream.Empty
    else from #:: datesStream( from.plusDays(1), to)
  }

  def datesStreamTimes(
      from: LocalDate,
      to: LocalDate,
      timesResolver: LocalDate => List[LocalTime]
  ): Stream[(LocalDate, List[LocalTime])] ={
    datesStream(from,to).map(x=>(x,timesResolver(x)))
  }



  def datesStreamTimesStream(
      from: LocalDate,
      to: LocalDate,
      timesResolver: LocalDate => Stream[LocalTime]
  ): Stream[(LocalDate, LocalTime)] =
    datesStream(from,to).flatMap(x =>(timesResolver(x).map(y=>(x,y))))

}