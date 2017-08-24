package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Visualization2Test extends FunSuite with Checkers {

  test("bilinearInterpolation") {
    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 1, 1, 1, 1) == 1)
    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 0, 1, 1, 1) == 0.75)
    assert(Visualization2.bilinearInterpolation(0.5, 0.5, 0, 0, 1, 1) == 0.5)
  }
}
