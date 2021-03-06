package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import observatory.Interaction.tileLocation
import observatory.Visualization.pixel

import scala.collection.immutable

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  val Dim = 256

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    val x0 = (1 - x) * d00 + x * d10
    val x1 = (1 - x) * d01 + x * d11

    (1 - y) * x0 + y * x1
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {

    val physicalPixelLocation = (i: Int, j: Int) => tileLocation(zoom + 8, i + x * Dim, j + y * Dim)

    val predictTemperature =
      (l: Location) => bilinearInterpolation(
        l.lon - l.lon.toInt, l.lat - l.lat.toInt,
        grid(l.lat.toInt, l.lon.toInt),
        grid(l.lat.toInt + 1, l.lon.toInt),
        grid(l.lat.toInt, l.lon.toInt + 1),
        grid(l.lat.toInt + 1, l.lon.toInt + 1)
      )

    val pixels: immutable.IndexedSeq[Pixel] =
      for (j <- 0 until Dim; i <- 0 until Dim) yield pixel(colors, physicalPixelLocation(i, j), predictTemperature)

    Image.apply(Dim, Dim, pixels.toArray)
  }
}
