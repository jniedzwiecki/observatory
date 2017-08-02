package observatory

import java.time.LocalDate

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {

  test("locateTemperaturesTest")  {
    val tuples: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(1975, "/stations_min.csv", "/1975_min.csv")

    val expectedTuples: List[(LocalDate, Location, Double)] = List(
      (LocalDate.of(1975, 1, 1), Location(69.056,018.540), 26.8),
      (LocalDate.of(1975, 1, 2), Location(69.056,018.540), 37.0),
      (LocalDate.of(1975, 1, 3), Location(69.056,018.540), 33.1),
      (LocalDate.of(1975, 1, 4), Location(69.056,018.540), 27.7),
      (LocalDate.of(1975, 1, 5), Location(69.056,018.540), 20.0),
      (LocalDate.of(1975, 1, 6), Location(69.056,018.540), 3.6),
      (LocalDate.of(1975, 1, 7), Location(69.056,018.540), 12.3),
      (LocalDate.of(1975, 1, 8), Location(69.056,018.540), 11.1),
      (LocalDate.of(1975, 1, 9), Location(69.056,018.540), -6.2),
      (LocalDate.of(1975, 1, 10), Location(69.056,018.540), -0.2),

      (LocalDate.of(1975, 1, 1), Location(69.683,018.919), 33.1),
      (LocalDate.of(1975, 1, 2), Location(69.683,018.919), 38.7),
      (LocalDate.of(1975, 1, 3), Location(69.683,018.919), 35.8),
      (LocalDate.of(1975, 1, 4), Location(69.683,018.919), 28.9),
      (LocalDate.of(1975, 1, 5), Location(69.683,018.919), 27.1),
      (LocalDate.of(1975, 1, 6), Location(69.683,018.919), 22.6),
      (LocalDate.of(1975, 1, 7), Location(69.683,018.919), 16.4),
      (LocalDate.of(1975, 1, 8), Location(69.683,018.919), 19.9),
      (LocalDate.of(1975, 1, 9), Location(69.683,018.919), 16.0),
      (LocalDate.of(1975, 1, 10), Location(69.683,018.919), 18.0)
    )

    assert(expectedTuples.size == tuples.size)
    assert(expectedTuples.forall(t => tuples.toList.contains(t)))
  }

  test("locationYearlyAverageRecords") {
    val tuples: Iterable[(LocalDate, Location, Double)] = Extraction.locateTemperatures(1975, "/stations_min.csv", "/1975_min.csv")

    val averageTuples: Iterable[(Location, Double)] = Extraction.locationYearlyAverageRecords(tuples)

    val expectedTuples: List[(Location, Double)] = List(
      (Location(69.056,018.540), 16.52),
      (Location(69.683,018.919), 25.65)
    )

    assert(expectedTuples.forall(t => expectedTuples.contains(t)))
  }
}