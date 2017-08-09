package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  test("tile location") {
    import Interaction.tileLocation

    assert(Location(85.05112877980659, -180.0) == tileLocation(0, 0, 0))
    assert(Location(85.05112877980659, -180.0) == tileLocation(1, 0, 0))
    assert(Location(85.05112877980659, -180.0) == tileLocation(2, 0, 0))
    assert(Location(85.05112877980659, -180.0) == tileLocation(3, 0, 0))
  }
}
