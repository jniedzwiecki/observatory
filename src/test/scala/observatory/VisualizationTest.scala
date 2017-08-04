package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("distance") {
    import Visualization.distance
    import math.toDegrees

    assert(distance(Location(0, 0), Location(toDegrees(0.5 * math.Pi), 0), 1) == math.Pi / 2)

    assert(distance(Location(0, 0), Location(toDegrees(0.5 * math.Pi), toDegrees(0.5 * math.Pi)), 1) == math.Pi / 2)

    assert(distance(Location(toDegrees(0.5 * math.Pi), 0), Location(toDegrees(- 0.5 * math.Pi), 0), 1) == math.Pi)
  }

  test("shortest distance and temp") {
    import Visualization.closestDistanceAndTemp
    import math.Pi

    val locations: List[Location] = List(Location(0, - Pi / 4), Location(0, 0), Location(0, Pi / 3), Location(0, Pi / 2))
    val locationTemp: List[(Location, Double)] = locations.zip(List(10.0, 20.0, 30.0, 40.0))

    val closestDistTemp: (Double, Double) = closestDistanceAndTemp(locationTemp, Location(Pi / 4, 0))
    assert(closestDistTemp._2 == 20)
  }

  test("predict temp") {
    import Visualization.predictTemperature
    import math.Pi

    val locations: List[Location] = List(Location(0, - Pi / 2), Location(0, Pi / 2))
    val locationTemp: List[(Location, Double)] = locations.zip(List(-10.0, 10.0))

    assert(predictTemperature(locationTemp, Location(0, 0)) == 0)
  }

  test("interpolate colors") {
    import Visualization.interpolateColor

    val color: Color = interpolateColor(
      List(
        (0, Color(0, 0, 0)),
        (1, Color(100, 100, 100)),
        (2, Color(200, 200, 200)),
        (3, Color(300, 300, 300))
      ),
      1.2
    )
    assert(color.red == 120 && color.green == 120 && color.blue == 120)
  }

  test("interpolate colors exceed scale") {
    import Visualization.interpolateColor

    val color: Color = interpolateColor(
      List(
        (0, Color(0, 0, 0)),
        (1, Color(100, 100, 100)),
        (2, Color(200, 200, 200)),
        (3, Color(300, 300, 300))
      ),
      3.2
    )
    assert(color.red == 300 && color.green == 300 && color.blue == 300)
  }

  test("interpolate colors exceed scale 2") {
    import Visualization.interpolateColor

    val color: Color = interpolateColor(
      List(
        (0, Color(0, 0, 0)),
        (1, Color(100, 100, 100)),
        (2, Color(200, 200, 200)),
        (3, Color(300, 300, 300))
      ),
      -0.5
    )
    assert(color.red == 0 && color.green == 0 && color.blue == 0)
  }

  test("interpolate colors 2") {
    import Visualization.interpolateColor

    val color: Color = interpolateColor(
      List(
        (0, Color(0, 0, 0)),
        (1, Color(100, 100, 100)),
        (2, Color(200, 200, 200)),
        (3, Color(300, 300, 300))
      ),
      3.0
    )
    assert(color.red == 300 && color.green == 300 && color.blue == 300)
  }
}
