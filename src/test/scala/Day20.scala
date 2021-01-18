package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day20Spec extends AnyFunSuite {
    val lines = Problem.parseInputToListOfLongArray("day20-test", "-")

    test("Day 20: mergeSegments") {
        assertResult(List((0L, 2L), (4L, 8L))) {
            Day20.mergeSegments(lines)
        }
    }

    test("Day 20: getLowest") {
        assertResult(3L) {
            Day20.getLowest(Day20.mergeSegments(lines), 0L)
        }
    }

    test("Day 20: getGapSum") {
        assertResult(1) {
            Day20.getGapSum(Day20.mergeSegments(lines))
        }
    }
}
