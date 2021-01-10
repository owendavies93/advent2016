package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day9Spec extends AnyFunSuite {

    test("Day 9: getLength") {
        var string = "ADVENT"
        assertResult(6) {
            Day9.getLength(string)
        }

        string = "A(1x5)BC"
        assertResult(7) {
            Day9.getLength(string)
        }

        string = "A(2x2)BCD(2x2)EFG"
        assertResult(11) {
            Day9.getLength(string)
        }

        string = "(6x1)(1x3)A"
        assertResult(6) {
            Day9.getLength(string)
        }

        string = "X(8x2)(3x3)ABCY"
        assertResult(18) {
            Day9.getLength(string)
        }
    }

    test("Day 9: getRecursiveLength") {
        var string = "(3x3)XYZ"
        assertResult(9) {
            Day9.getRecursiveLength(string)
        }

        string = "X(8x2)(3x3)ABCY"
        assertResult(20) {
            Day9.getRecursiveLength(string)
        }

        string = "(27x12)(20x12)(13x14)(7x10)(1x12)A"
        assertResult(241920) {
            Day9.getRecursiveLength(string)
        }

        string = "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
        assertResult(445) {
            Day9.getRecursiveLength(string)
        }
    }
}
