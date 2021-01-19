package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day22Spec extends AnyFunSuite {

    test("Day 22: parsing") {
        val l = "/dev/grid/node-x10-y12   86T   72T    14T   83%"
        val n = Day22.parseLine(l)

        assertResult(10) {
            n.x
        }

        assertResult(72) {
            n.used
        }
    }
}
