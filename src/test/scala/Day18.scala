package advent2016

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class Day18Spec extends AnyFunSuite {

    test("TileRow: getNext()") {
        var b = ArrayBuffer(false, false, true, true, false)
        var t = new TileRow(b, 5, 1)
        t = t.getNext()
        t = t.getNext()

        assertResult(9) {
            t.countOn()
        }

        b = ArrayBuffer(false, true, true, false, true, false, true, true, true, true)
        t = new TileRow(b, b.size, 1)

        (1 until 10).foreach(n => t = t.getNext() )

        assertResult(38) {
            t.grid.size - t.countOn()
        }
    }
}
