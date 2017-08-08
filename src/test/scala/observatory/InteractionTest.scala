package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  test("tile location") {
    val location: Location = Interaction.tileLocation(1, 1, 1)
    val x = 0
  }
}
