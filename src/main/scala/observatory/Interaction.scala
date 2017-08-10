package observatory

import com.sksamuel.scrimage.Image

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val TileSize = 256

  /**
    * @param zoom Zoom level
    * @param x    X coordinate
    * @param y    Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val count: Double = math.pow(2.0, zoom)
    val lon: Double = math.toRadians(x / count * 360.0 - 180)
    val n: Double = Math.PI - (2.0 * Math.PI * y) / count
    val lat: Double = math.atan(math.sinh(n))
    Location(math.toDegrees(lat), math.toDegrees(lon))
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
    tileDim(temperatures, colors, zoom, x, y, TileSize)
  }

  def tileDim(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int, dim: Int): Image = {
    val pixelLocation = (i: Int, j: Int) => Some(tileLocation(zoom + 8, i, j)).map(l => Location(math.toRadians(l.lat), math.toRadians(l.lon))).get

    Visualization.visualizeDim(temperatures, colors,
      pixelLatRange(parentOffset(zoom, y), dim),
      pixelLonRange(parentOffset(zoom, x), dim),
      dim, dim,
      (i: Int, j: Int) => pixelLocation(i, j))
  }

  def pixelLonRange(parentUpperLeftX: Int, dim: Int): Range = {
    new Range(parentUpperLeftX * dim, (parentUpperLeftX + 1) * dim, 1)
  }

  def pixelLatRange(parentUpperLeftY: Int, dim: Int): Range = {
    new Range((parentUpperLeftY + 1) * dim, parentUpperLeftY * dim, -1)
  }

  def parentOffset(zoom: Int, parentCoordinate: Int) = parentCoordinate * TileSize

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
