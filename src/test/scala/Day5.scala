package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day5Spec extends AnyFunSuite {

    test("Day 5: findZeroHashChar") {
        val id = "abc"

        assertResult('1') {
            Day5.findZeroHashChar(id, 3000000)._2
        }
    }
}
