package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  test("distance") {
    val l1 = Location(0, 0)
    val l2 = Location(90, 0)

    val d: Double = Visualization.distance(l1, l2, 1)
    assert(d == 3.14)
  }
}
