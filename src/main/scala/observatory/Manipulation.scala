package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  val grid: Array[Array[Option[Double]]] = Array.fill[Option[Double]](360, 180)(None)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    (lat: Int, lon: Int) => {
      require((-89 to 90) contains lat, f"latitude outside the range: $lat")
      require((-180 to 179) contains lon, f"longitude outside the range: $lon")
      if (grid(lon + 180)(lat + 90).isEmpty) grid(lon + 180)(lat + 90) = Some(Visualization.predictTemperature(temperatures, Location(lat, lon)))
      grid(lon + 180)(lat + 90).get
    }
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    (lat: Int, lon: Int) => temperaturess.map(makeGrid).map(_(lat, lon)).sum / temperaturess.size
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    (lat: Int, lon: Int) => makeGrid(temperatures)(lat, lon) - normals(lat, lon)
  }
}

