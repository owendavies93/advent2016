package advent2016

import scalaadventutils.WeightedUndirectedGraph

import org.scalatest.funsuite.AnyFunSuite

class Day13Spec extends AnyFunSuite {
    test("Day 13: constructGrid") {
        val g = Day13.constructGrid(10, 10)

        assert(g.get(0, 0))
        assert(!g.get(1, 0))
    }

    test("Day13: constructGraph") {
        val g = Day13.constructGraph(Day13.constructGrid(10, 10))

        assertResult(Map((0, 1) -> 1)) {
            g((0, 0))
        }

        assertResult(Map((6, 4) -> 1, (7, 5) -> 1, (5, 5) -> 1, (6, 6) -> 1)) {
            g((6, 5))
        }
    }

    test("Day 13: getShortestPath") {
        val g = Day13.constructGraph(Day13.constructGrid(10, 10))

        assertResult(11) {
            Day13.getShortestPath(g, (7, 4))
        }
    }

    test("Day 13: part2") {
        val g = Day13.constructGraph(Day13.constructGrid(10, 10))

        assertResult(6) {
            Day13.part2(g, 10, 3)
        }

        assertResult(11) {
            Day13.part2(g, 10, 5)
        }
    }
}
