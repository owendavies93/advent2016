package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day16Spec extends AnyFunSuite {

    test("Day 16: dragonCurve") {

        assertResult("100") {
            Day16.dragonCurve("1")
        }

        assertResult("001") {
            Day16.dragonCurve("0")
        }

        assertResult("11111000000") {
            Day16.dragonCurve("11111")
        }

        assertResult("1111000010100101011110000") {
            Day16.dragonCurve("111100001010")
        }
    }

    test("Day 16: fillDisk") {

        assertResult("10000011110010000111") {
            Day16.fillDisk(20, "10000")
        }
    }

    test("Day 16: checksumUntilEven") {

        assertResult("100") {
            Day16.checksumUntilOdd("110010110100")
        }

        assertResult("01100") {
            Day16.checksumUntilOdd(Day16.fillDisk(20, "10000"))
        }
    }
}
