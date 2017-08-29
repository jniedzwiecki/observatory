package observatory

import scala.collection.immutable

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val averageTemperatures: immutable.IndexedSeq[Iterable[(Location, Double)]] =
      for (year <- 1975 to 1989) yield
        Extraction.locateTemperatures(year, "/stations.csv", f"/$year.csv")
          .map { case (date, location, temp) => (location, temp) }

    val normals: (Int, Int) => Double = Manipulation.average(averageTemperatures)

    val yearDeviations: Iterable[(Int, (Int, Int) => Double)] = for (year <- 1990 to 2015) yield {
      val temperatures: Iterable[(Location, Double)] = Extraction.locateTemperatures(year, "/stations.csv", f"/$year.csv")
        .map { case (date, location, temp) => (location, temp) }
      (year, Manipulation.deviation(temperatures, normals))
    }

    val colors: List[(Double, Color)] = List(
      (7, Color(0, 0, 0)),
      (4, Color(255, 0, 0)),
      (2, Color(255, 255, 0)),
      (0, Color(255, 255, 255)),
      (-2, Color(0, 255, 255)),
      (-7, Color(0, 0, 255))
    )

    Interaction.generateTiles(
      yearDeviations,
      (year, zoom, x, y, data: (Int, Int) => Double) => {
        Visualization2.visualizeGrid(
          (i: Int, j: Int) => data(i, j) - normals(i, j),
          colors,
          zoom,
          x,
          y
        ).output(year + ".png")
      }
    )
  }
}
