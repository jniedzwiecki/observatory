package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.collection.immutable

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  private val InvertedDistancePower: Int = 2

  private val MinimumDistance: Int = 1
  private val EarthRadius: Double = 6371
  var Width: Int = 360
  var Height: Int = 180

  val Alpha = 0x7F

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
    points.find { case (temp, _) => temp == value }.map(_._2).getOrElse {
      if (!points.exists { case (temp, _) => temp < value }) points.minBy(_._1)._2
      else if (!points.exists { case (temp, _) => temp > value }) points.maxBy(_._1)._2
      else {
        val left: (Double, Color) = points.filter { case (temp, _) => temp < value }.maxBy(_._1)
        val right: (Double, Color) = points.filter { case (temp, _) => temp > value }.minBy(_._1)

        val red: Int = colorRatio((left._1, left._2.red), (right._1, right._2.red), value)
        val green: Int = colorRatio((left._1, left._2.green), (right._1, right._2.green), value)
        val blue: Int = colorRatio((left._1, left._2.blue), (right._1, right._2.blue), value)
        Color(red, green, blue)
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    visualizeDim(temperatures, colors,
      (Height / 2) to (-Height / 2 + 1) by -1,
      (-Width / 2) until (Width / 2),
      Width, Height,
      (i: Int, j: Int) => Location(i, j),
      location => predictTemperature(temperatures, location))
  }

  def visualizeDim(temperatures: Iterable[(Location, Double)],
                   colors: Iterable[(Double, Color)],
                   pixelLatRange: Range,
                   pixelLonRange: Range,
                   width: Int, height: Int,
                   pixelLocation: (Int, Int) => Location,
                   predictTemperature: (Location) => Double): Image = {
    val pixels: immutable.IndexedSeq[Pixel] =
      for (i <- pixelLatRange; j <- pixelLonRange) yield
        pixel(colors, pixelLocation(i, j), predictTemperature)

    Image.apply(width, height, pixels.toArray)
  }

  def pixel(colors: Iterable[(Double, Color)],
                    location: Location,
                    predictTemperature: (Location) => Double) = {
    val color: Color = interpolateColor(colors, predictTemperature(location))
    Pixel.apply(color.red, color.green, color.blue, Alpha)
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
    import math._

    radius * acos(sin(toRadians(l1.lat)) * sin(toRadians(l2.lat)) + cos(toRadians(l1.lat)) * cos(toRadians(l2.lat)) * cos(abs(toRadians(l1.lon) - toRadians(l2.lon))))
  }

  def colorRatio(left: (Double, Int), right: (Double, Int), value: Double): Int =
    math.round((left._2 * (right._1 - value) + right._2 * (value - left._1)) / (right._1 - left._1)).toInt

  def colorToInt(color: Color): Int = {
    val red: Int = (color.red << 16) & 0x00FF0000
    val green: Int = (color.green << 8) & 0x0000FF00
    val blue: Int = color.blue & 0x000000FF

    0xFF000000 | red | green | blue
  }
}

