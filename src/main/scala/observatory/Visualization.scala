package observatory

import com.sksamuel.scrimage.{Image, Pixel}

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
    ???
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    ???
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
}

