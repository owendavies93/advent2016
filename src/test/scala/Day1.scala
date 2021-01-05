package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day1Spec extends AnyFunSuite {

    test("Day 1: nextLocation") {
        val start = new Location(0, 0, Direction.N)
        val next = Day1.nextLocation(start, 'R', 2)

        assertResult(0) {
            next.x
        }

        assertResult(2) {
            next.y
        }

        assertResult(Direction.E) {
            next.dir
        }
    }

    test("Day 1: part1") {
        var dirs = List(('R', 2), ('L', 3))

        assertResult(5) {
            Day1.part1(dirs)
        }

        dirs = List(('R', 2), ('R', 2), ('R', 2))

        assertResult(2) {
            Day1.part1(dirs)
        }

        dirs = List(('R', 5), ('L', 5), ('R', 5), ('R', 3))

        assertResult(12) {
            Day1.part1(dirs)
        }
    }

    test("Day 1: pointsBetweenPoints") {
        var start = new Location(0, 0, Direction.N)
        var end = new Location(0, 4, Direction.N)

        assertResult(3) {
            Day1.pointsBetweenPoints(start, end).size
        }

        end = new Location(2, 0, Direction.N)
        val ps = Day1.pointsBetweenPoints(start, end)
        assertResult(1) {
            ps.size
        }

        assertResult(1) {
            ps(0).x
        }

        end = Day1.nextLocation(start, 'R', 1)
        assert(Day1.pointsBetweenPoints(start, end).isEmpty)

        start = new Location(0, 8, Direction.E)
        end = new Location(-4, 8, Direction.E)

        assertResult(3) {
            Day1.pointsBetweenPoints(start, end).size
        }
    }

    test("Day 1: part 2") {
        val dirs = List(('R', 8), ('R', 4), ('R', 4), ('R', 8))

        assertResult(4) {
            Day1.part2(dirs)
        }
    }
}
