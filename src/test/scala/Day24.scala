package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day24Spec extends AnyFunSuite {
    test("Day24: constructGrid") {
        val input = Problem.parseInputToList("day24-test")

        val (g, pointMap) = Day24.constructGrid(input)

        assert(!g.get(0, 0))
        assert(!g.get(0, 4))
        assert(g.get(1, 1))
        assert(g.get(9, 1))

        assertResult((1, 1)) {
            pointMap(0)
        }

        assertResult((1, 3)) {
            pointMap(4)
        }
    }

    test("Day 24: part1") {
        val input = Problem.parseInputToList("day24-test")

        assertResult(14) {
            Day24.part1(input)
        }
    }

    test("Day 24: part2") {
        val input = Problem.parseInputToList("day24-test")

        assertResult(20) {
            Day24.part2(input)
        }
    }
}
