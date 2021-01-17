package advent2016

import scalaadventutils.CellulaAutomata

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

class Day18Spec extends AnyFunSuite {

    test("Day 18: getOn()") {
        var b = ArrayBuffer(false, false, true, true, false)
        var ca = new CellulaAutomata(b, b.size, 1)

        assertResult(2) {
            ca.countOn()
        }

        assertResult(9) {
            Day18.getOn(ca, 3)
        }

        b = ArrayBuffer(false, true, true, false, true, false, true, true, true, true)
        ca = new CellulaAutomata(b, b.size, 1)

        assertResult(38) {
            b.size * 10 - Day18.getOn(ca, 10)
        }
    }
}
