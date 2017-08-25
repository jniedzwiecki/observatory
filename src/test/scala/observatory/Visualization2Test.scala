package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Visualization2Test extends FunSuite with Checkers {

  test("bilinearInterpolation") {
    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 1, 1, 1, 1) == 1)
    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 0, 1, 1, 1) == 0.75)
    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 0, 0, 1, 1) == 0.5)
  }

  test("bilinearInterpolation2") {
    assert(Visualization2.bilinearInterpolation(0, 0, -2, 100, 100, 100) == -2)
  }

  test("visualizeGrid") {
    val locations: List[(Location, Double)] = List(
      (Location(45.0, -90.0), 88.95301869434877),
      (Location(-45.0, 0.0), -42.759401365885516)
    )

    val colors: List[(Double, Color)] = List(
      (60, Color(255, 255, 255)),
      (32, Color(255, 0, 0)),
      (12, Color(255, 255, 0)),
      (0, Color(0, 255, 255)),
      (-15, Color(0, 0, 255)),
      (-27, Color(255, 0, 255)),
      (-50, Color(33, 0, 107)),
      (-60, Color(0, 0, 0))
    )

    val grid = Manipulation.makeGrid(locations)

    val image = Visualization2.visualizeGrid(grid, colors, 0, 0, 0)
    image.output("visualization.png")
  }

  test("grid") {
    val locations: List[(Location, Double)] = List(
      (Location(45.0, -90.0), 88.95301869434877),
      (Location(-45.0, 0.0), -42.759401365885516)
    )

    val grid = Manipulation.makeGrid(locations)
    val d = grid(20, 30)
    print(d)
  }
}
