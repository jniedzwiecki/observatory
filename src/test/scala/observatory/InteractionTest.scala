package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.math.Pi

trait InteractionTest extends FunSuite with Checkers {

  test("tile location") {
    import Interaction.tileLocation

    assert(tileLocation(0, 0, 0) == Location(85.05112877980659, -180.0))
    assert(tileLocation(1, 0, 0) == Location(85.05112877980659, -180.0))
    assert(tileLocation(2, 0, 0) == Location(85.05112877980659, -180.0))
    assert(tileLocation(3, 0, 0) == Location(85.05112877980659, -180.0))

    assert(tileLocation(0, 1, 0) == Location(85.05112877980659, 180.0))
    assert(tileLocation(0, 0, 1) == Location(-85.05112877980659, -180.0))
    assert(tileLocation(0, 1, 1) == Location(-85.05112877980659, 180.0))

    assert(tileLocation(1, 1, 0) == Location(85.05112877980659, 0.0))
    assert(tileLocation(1, 0, 1) == Location(0, -180.0))
    assert(tileLocation(1, 1, 1) == Location(0, 0.0))

  }

  test("tile zoom 0") {
    import Interaction.tileDim

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

    val locations: List[(Location, Double)] = List(
      (Location(Pi / 4, - Pi / 2), 60),
      (Location(- Pi / 4, Pi / 2), -60)
    )

    tileDim(locations, colors, 0, 0, 0, 256).output("tile00.png")
  }

  test("#3") {
    import Interaction.tileDim

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

    val locations: List[(Location, Double)] = List(
      (Location(45, -90), 10),
      (Location(-45, 0), 20)
    )

    tileDim(locations, colors, 0, 0, 0, 256).output("tile00.png")
    tileDim(locations, colors, 1, 1, 1, 256).output("tile11.png")
  }

  test("#3#2") {
    import Interaction.tileDim

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

    val locations: List[(Location, Double)] = List(
      (Location(45.0, -90.0), 88.95301869434877),
      (Location(-45.0, 0.0), -42.759401365885516)
    )

    tileDim(locations, colors, 0, 0, 0, 256).output("tile#0#00.png")
    tileDim(locations, colors, 1, 0, 0, 256).output("tile#00.png")
    tileDim(locations, colors, 1, 1, 0, 256).output("tile#10.png")
    tileDim(locations, colors, 1, 0, 1, 256).output("tile#01.png")
    tileDim(locations, colors, 1, 1, 1, 256).output("tile#11.png")
  }
}
