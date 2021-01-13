package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day12Spec extends AnyFunSuite {

    test("Day 12: run") {
        val input = Problem.parseInputToList("day12-test")

        val out = Day12.run(input)
        assertResult(42) {
            out("a")
        }
    }

}
