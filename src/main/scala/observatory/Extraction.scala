package observatory

import java.time.LocalDate

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    def stations: List[String] = Source.fromURL(getClass.getResource(stationsFile)).getLines().toList
    def temperatures: List[String] = Source.fromURL(getClass.getResource(temperaturesFile)).getLines().toList

    locateTemperatures(year, stations, temperatures)
  }


  def locateTemperatures(year: Int, stations: List[String], temperatures: List[String]): Iterable[(LocalDate, Location, Double)] = {
    temperatures.map(_.split(',')).map {
      case Array(stn: String, wban: String, month: String, day: String, temp: String) => (
        LocalDate.of(year, month.toInt, day.toInt),
        stationLocationByStnOrWban(stations, strToOptStr(stn), strToOptStr(wban)).map {
          case (lat, lon) => Location(lat.toDouble, lon.toDouble)
        },
        temp.toDouble
      )
    }.flatMap { case (date, location, temp) => if (location.isDefined) Some((date, location.get, toCelsius(temp))) else None }
  }

  def toCelsius(fahrenheitTemp: Double) = (fahrenheitTemp - 32.0) * 5.0 / 9.0

  def stationLocationByStnOrWban(stations: List[String], stn: Option[String], wban: Option[String]): Option[(Double, Double)] = {
    val station: Option[Array[String]] =
    stations.map(_.split(",", -1))
      .find(l => l(0).equals(stn.getOrElse(""))
    && l(1).equals(wban.getOrElse(""))
    && l(2).length > 0
    && l(3).length > 0)
    station.flatMap {
      case Array(_, _, lat: String, lon: String) => Some((lat.toDouble, lon.toDouble))
      case _ => None
    }
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val grouped: Map[Location, Iterable[(LocalDate, Location, Double)]] = records.groupBy { case (_, location, _) => location }
    val tuples: List[(Location, Double)] = grouped.toList
      .map { case (location: Location, records: Iterable[(LocalDate, Location, Double)]) => (location, records.foldLeft(0.0)((acc, el) => acc + el._3) / records.size) }
    tuples
  }

  def strToOptStr(str: String): Option[String] = valToOptVal(str, () => !str.isEmpty)

  def valToOptVal[T](t: T, f: () => Boolean): Option[T] = if (f()) Some(t) else None
}
