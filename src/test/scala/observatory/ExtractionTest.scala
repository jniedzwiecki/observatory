package observatory

import java.time.LocalDate

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {
  import Extraction.locateTemperatures
  import Extraction.locationYearlyAverageRecords


  test("locateTemperaturesTest")  {
    import Extraction.toCelsius

    val tuples: Iterable[(LocalDate, Location, Double)] = locateTemperatures(1975, "/stations_min.csv", "/1975_min.csv")

    val expectedTuples: List[(LocalDate, Location, Double)] = List(
      (LocalDate.of(1975, 1, 1), Location(69.056,018.540), toCelsius(26.8)),
      (LocalDate.of(1975, 1, 2), Location(69.056,018.540), toCelsius(37.0)),
      (LocalDate.of(1975, 1, 3), Location(69.056,018.540), toCelsius(33.1)),
      (LocalDate.of(1975, 1, 4), Location(69.056,018.540), toCelsius(27.7)),
      (LocalDate.of(1975, 1, 5), Location(69.056,018.540), toCelsius(20.0)),
      (LocalDate.of(1975, 1, 6), Location(69.056,018.540), toCelsius(3.6)),
      (LocalDate.of(1975, 1, 7), Location(69.056,018.540), toCelsius(12.3)),
      (LocalDate.of(1975, 1, 8), Location(69.056,018.540), toCelsius(11.1)),
      (LocalDate.of(1975, 1, 9), Location(69.056,018.540), toCelsius(-6.2)),
      (LocalDate.of(1975, 1, 10), Location(69.056,018.540), toCelsius(-0.2)),

      (LocalDate.of(1975, 1, 1), Location(69.683,018.919), toCelsius(33.1)),
      (LocalDate.of(1975, 1, 2), Location(69.683,018.919), toCelsius(38.7)),
      (LocalDate.of(1975, 1, 3), Location(69.683,018.919), toCelsius(35.8)),
      (LocalDate.of(1975, 1, 4), Location(69.683,018.919), toCelsius(28.9)),
      (LocalDate.of(1975, 1, 5), Location(69.683,018.919), toCelsius(27.1)),
      (LocalDate.of(1975, 1, 6), Location(69.683,018.919), toCelsius(22.6)),
      (LocalDate.of(1975, 1, 7), Location(69.683,018.919), toCelsius(16.4)),
      (LocalDate.of(1975, 1, 8), Location(69.683,018.919), toCelsius(19.9)),
      (LocalDate.of(1975, 1, 9), Location(69.683,018.919), toCelsius(16.0)),
      (LocalDate.of(1975, 1, 10), Location(69.683,018.919), toCelsius(18.0))
    )

    assert(expectedTuples.size == tuples.size)
    assert(expectedTuples.forall(t => tuples.toList.contains(t)))
  }

  test("locationYearlyAverageRecords") {
    val tuples: Iterable[(LocalDate, Location, Double)] = locateTemperatures(1975, "/stations_min.csv", "/1975_min.csv")

    val averageTuples: Iterable[(Location, Double)] = locationYearlyAverageRecords(tuples)

    val expectedTuples: List[(Location, Double)] = List(
      (Location(69.056,018.540), 16.52),
      (Location(69.683,018.919), 25.65)
    )

    assert(expectedTuples.forall(t => expectedTuples.contains(t)))
  }

  test("error1") {
    val tuples = locateTemperatures(2000, List("111111,,1.0,-1.0"), List("111111,,01,01,10.0"))
    tuples.foreach(l => println(l._1 + " " + l._2 + " " + l._3))
  }

  test("stn wban") {
    val tuples = locateTemperatures(2000, "/stations_min2.csv", "/1975_min2.csv")

    tuples
  }
}