package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day2Spec extends AnyFunSuite {

    test("Day 2: tracePath") {
        var path = "ULL"

        assertResult(1) {
            Day2.tracePath(path, 5)
        }

        path = "RRDDD"

        assertResult(9) {
            Day2.tracePath(path, 1)
        }

        path = "LURDL"

        assertResult(8) {
            Day2.tracePath(path, 9)
        }

        path = "UUUUD"

        assertResult(5) {
            Day2.tracePath(path, 8)
        }
    }

    test("Day2: tracePath2") {
        var path = "ULL"

        assertResult('5') {
            Day2.tracePath2(path, '5')
        }

        path = "RRDDD"

        assertResult('D') {
            Day2.tracePath2(path, '5')
        }

        path = "LURDL"

        assertResult('B') {
            Day2.tracePath2(path, 'D')
        }

        path = "UUUUD"

        assertResult('3') {
            Day2.tracePath2(path, 'B')
        }
    }
}
