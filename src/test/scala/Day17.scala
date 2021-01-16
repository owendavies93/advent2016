package advent2016

import org.scalatest.funsuite.AnyFunSuite

import scalaadventutils.Hashing
import advent2016.Dir._

class Day17Spec extends AnyFunSuite {

    test("Day 17: getAllowedDirs") {
        var pass = "hijkl"

        assertResult(List(U, D, L)) {
            Day17.getAllowedDirs(Hashing.md5AsString(pass))
        }

        assertResult(List((new Point(0, 1), D))) {
            Day17.getValidMoves(new Point(0, 0), pass)
        }

        pass = "hijklD"

        assertResult(List((new Point(0, 0), U), (new Point(1, 1), R))) {
            Day17.getValidMoves(new Point(0, 1), pass)
        }
    }

    test("Day 17: getPath") {
        var pass = "hijkl"

        assertResult(List()) {
            Day17.getPath(new Point(0, 0), new Point(3, 3), pass)
        }

        pass = "ihgpwlah"

        assertResult("DDRRRD") {
            Day17.getPath(new Point(0, 0), new Point(3, 3), pass)(0)
        }

        pass = "kglvqrro"

        assertResult("DDUDRLRRUDRD") {
            Day17.getPath(new Point(0, 0), new Point(3, 3), pass)(0)
        }

        pass = "ulqzkmiv"

        assertResult("DRURDRUDDLLDLUURRDULRLDUUDDDRR") {
            Day17.getPath(new Point(0, 0), new Point(3, 3), pass)(0)
        }
    }
}
