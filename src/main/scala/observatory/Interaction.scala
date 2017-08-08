package observatory

import com.sksamuel.scrimage.Image

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val Dim = 256

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val tileCount: Double = math.pow(2.0, zoom)
    val lon: Double = x / Math.pow(2.0, tileCount) * 360.0 - 180
    val n: Double = Math.PI - (2.0 * Math.PI * y) / Math.pow(2.0, tileCount)
    val lat: Double = math.toDegrees(math.atan(math.sinh(n)))
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param zoom         Zoom level
    * @param x            X coordinate
    * @param y            Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    Visualization.visualizeDimension(temperatures, colors,
      new Range((x + 1) * Dim, x * Dim, -1),
      new Range(y * Dim, (y + 1) * Dim, 1),
      Dim, Dim, (i: Int, j: Int) => tileLocation(zoom + 8, i, j))
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for (year <- 1975 to 2015) yield
      for (zoom <- 0 to 3) yield
        for (x <- 0 until math.pow(2, zoom).toInt; y <- 0 until math.pow(2, zoom).toInt) yield {
          val data: Option[Data] = yearlyData.find(_._1 == year).map(_._2)
          data.foreach(generateImage(year, zoom, x, y, _))
        }
  }

}
