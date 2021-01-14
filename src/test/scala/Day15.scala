package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day15Spec extends AnyFunSuite {

    test("Day 15: part 1") {
        val input = Problem.parseInputToList("day15-test")
        val discs = input.map(Day15.parseInput)

        assertResult(5) {
            Day15.part1(discs)
        }
    }
}
