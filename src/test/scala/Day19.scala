package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day19Spec extends AnyFunSuite {

    test("Day 19: part 1") {
        assertResult(3) {
            Day19.josephus(5)
        }
    }

    test("Day 19: part 2") {
        assertResult(2) {
            Day19.part2(5)
        }
    }
}
