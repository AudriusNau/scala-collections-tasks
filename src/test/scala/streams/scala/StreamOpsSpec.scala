package streams.scala

import java.time.{LocalDate, LocalTime}

import org.scalatest.{Matchers, WordSpec}
import java.time.DayOfWeek
class StreamOpsSpec extends WordSpec with Matchers {

  val streamOps = new StreamOps

  "StreamOps" should {
    "valid int stream" in {
      streamOps
        .intStream(3, 2)
        .toList shouldEqual List(3, 4)

      streamOps
        .intStream(5, 5)
        .toList shouldEqual List(5, 6, 7, 8, 9)
    }

    "valid int mult stream" in {
      streamOps
        .intMultStream(3, 2, (el: Int) => el * 2)
        .toList shouldEqual List(6, 8)
    }

    "valid median of int stream" in {
      streamOps
        .intStreamMedian(5, 2) shouldEqual 5.5
    }

    "valid dates stream" in {
      val expected = List(
        LocalDate.of(2018, 1, 1),
        LocalDate.of(2018, 1, 2),
        LocalDate.of(2018, 1, 3),
        LocalDate.of(2018, 1, 4),
        LocalDate.of(2018, 1, 5)
      )
      streamOps
        .datesStream(
          LocalDate.of(2018, 1, 1),
          LocalDate.of(2018, 1, 5)
        )
        .toList shouldEqual expected
    }

    "valid dates stream times" in {
      /**
        * Should pick 9:30 and 10:00 times for weekdays and 7:00 to weekends
        */
      val timesResolver: LocalDate => List[LocalTime] =  {( timesResolver : LocalDate)=> (
        timesResolver.getDayOfWeek match{
          case DayOfWeek.MONDAY => List(LocalTime.of(9, 30),
            LocalTime.of(10, 0))
          case DayOfWeek.TUESDAY => List(LocalTime.of(9, 30),
            LocalTime.of(10, 0))
          case DayOfWeek.WEDNESDAY => List(LocalTime.of(9, 30),
            LocalTime.of(10, 0))
          case DayOfWeek.THURSDAY => List(LocalTime.of(9, 30),
            LocalTime.of(10, 0))
          case DayOfWeek.FRIDAY => List(LocalTime.of(9, 30),
            LocalTime.of(10, 0))
          case DayOfWeek.SATURDAY => List(LocalTime.of(7, 0))
          case DayOfWeek.SUNDAY => List(LocalTime.of(7, 0))
        }
      )
      }

      {
        val expected = List(
          (
            LocalDate.of(2018, 1, 5),
            List(
              LocalTime.of(9, 30),
              LocalTime.of(10, 0)
            )
          ),
          (
            LocalDate.of(2018, 1, 6),
            List(LocalTime.of(7, 0))
          )
        )

        streamOps
          .datesStreamTimes(
            LocalDate.of(2018, 1, 5),
            LocalDate.of(2018, 1, 6),
            timesResolver
          )
          .toList shouldEqual expected
      }
    }

    "valid dates stream times stream" in {
      /**
        * Should pick 9:30 and 10:00 times for weekdays and 7:00 to weekends
        */
      val timesResolver: LocalDate => Stream[LocalTime] = ???

      {
        val expected = List(
          (
            LocalDate.of(2018, 1, 5),
            LocalTime.of(9, 30)
          ),
          (
            LocalDate.of(2018, 1, 5),
            LocalTime.of(10, 0)
          ),
          (
            LocalDate.of(2018, 1, 6),
            LocalTime.of(7, 0)
          )
        )

        streamOps
          .datesStreamTimesStream(
            LocalDate.of(2018, 1, 5),
            LocalDate.of(2018, 1, 6),
            timesResolver
          )
          .toList shouldEqual expected
      }
    }
  }
}
