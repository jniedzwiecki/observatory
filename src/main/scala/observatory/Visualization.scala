package observatory

import java.awt.image.BufferedImage

import com.sksamuel.scrimage.{Image, ImageMetadata, Pixel}
import com.sun.javafx.iio.ImageStorage.ImageType

import scala.collection.immutable

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  private val InvertedDistancePower: Int = 2
  private val MinimumDistance: Int = 1
  private val EarthRadius: Double = 6371

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val closest: (Double, Double) = closestDistanceAndTemp(temperatures, location)

    if (closest._1 < MinimumDistance) closest._2
    else {
      val weightTempSum: Double = temperatures.foldLeft(0.0)((acc, temp) => acc + invertedDistancePower(location, temp._1, InvertedDistancePower) * temp._2)
      val weightSum: Double = temperatures.foldLeft(0.0)((acc, temp) => acc + invertedDistancePower(location, temp._1, InvertedDistancePower))
      weightTempSum / weightSum
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    if (!points.exists { case (temp, _) => temp < value }) points.minBy(_._1)._2
    else if (!points.exists { case (temp, _) => temp > value }) points.maxBy(_._1)._2
    else {
      val left: (Double, Color) = points.filter { case (temp, _) => temp <= value }.maxBy(_._1)
      val right: (Double, Color) = points.filter { case (temp, _) => temp >= value }.minBy(_._1)
      val red: Int = colorRatio((left._1, left._2.red), (right._1, right._2.red), value)
      val green: Int = colorRatio((left._1, left._2.green), (right._1, right._2.green), value)
      val blue: Int = colorRatio((left._1, left._2.blue), (right._1, right._2.blue), value)
      Color(red, green, blue)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val image: BufferedImage = new BufferedImage(360, 180, BufferedImage.TYPE_INT_RGB)
    val pixels: immutable.IndexedSeq[Color] = for (j <- 0 to 179; i <- 0 to 359) yield
      interpolateColor(colors, predictTemperature(temperatures, locationByPixel(i, j)))
    val ints: immutable.IndexedSeq[Int] = pixels.map(colorToInt(_))

    image.setRGB(0, 0, 360, 180, ints.toArray, 0, 0)

    new Image(image, ImageMetadata.fromImage(image))
  }

  def closestDistanceAndTemp(temperatures: Iterable[(Location, Double)], location: Location): (Double, Double) = {
    temperatures.foldLeft((Double.MaxValue, Double.MaxValue))(
      (currClosestDistanceAndTemp, pointAndTemp) => {
        val d: Double = distance(location, pointAndTemp._1, EarthRadius)
        if (d < currClosestDistanceAndTemp._1)
          (d, pointAndTemp._2)
        else
          currClosestDistanceAndTemp
      }
    )
  }

  def invertedDistancePower(l1: Location, l2: Location, power: Int): Double = {
    1 / math.pow(distance(l1, l2, EarthRadius), power)
  }

  def distance(l1: Location, l2: Location, radius: Double): Double = {
    import math.acos
    import math.sin
    import math.cos
    import math.abs
    import math.toRadians

    radius * acos(sin(toRadians(l1.lat)) * sin(toRadians(l2.lat)) + cos(toRadians(l1.lat)) * cos(toRadians(l2.lat)) * cos(abs(toRadians(l1.lon) - toRadians(l2.lon))))
  }

  def colorRatio(left: (Double, Int), right: (Double, Int), value: Double): Int =
    math.round((left._2 * (right._1 - value) + right._2 * (value - left._1)) / (right._1 - left._1)).toInt

  def locationByPixel(i: Int, j: Int): Location = ???

  def colorToInt(color: Color): Int = ???
}

