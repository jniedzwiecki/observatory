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

    assert(tileLocation(0, 1, 0) == Location(85.05112877980659, 0.0))
    assert(tileLocation(0, 0, 1) == Location(0.0, -180.0))
    assert(tileLocation(0, 1, 1) == Location(0.0, 0.0))
    assert(tileLocation(0, 2, 2) == Location(-85.05112877980659,180.0))

    assert(tileLocation(1, 1, 0) == Location(85.05112877980659, -90.0))
    assert(tileLocation(1, 0, 1) == Location(66.51326044311186, -180.0))
    assert(tileLocation(1, 1, 1) == Location(66.51326044311186, -90.0))
  }

  test("tile zoom 0") {
    import Interaction.tile

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
      (Location(- Pi / 2, - Pi / 2), 60),
      (Location(- Pi / 2 + 0.001, - Pi / 2), -60)
    )
//    val locations: List[(Location, Double)] = List(
//      (Location(Pi / 4, - Pi / 2), -60),
//      (Location(- Pi / 4, - Pi / 2), -60),
//      (Location(Pi / 4, Pi / 2), -60),
//      (Location(- Pi / 4, Pi / 2), -60)
//    )

    tile(locations, colors, 0, 0, 0).output("tile00.png")
  }
}
