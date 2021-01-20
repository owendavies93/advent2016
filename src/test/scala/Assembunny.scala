package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class AssembunnySpec extends AnyFunSuite {

    test("Assembunny: run") {
        val input = Problem.parseInputToList("day12-test")

        val m = Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
        val a = new Assembunny(m)
        val out = a.run(input)

        assertResult(42) {
            out("a")
        }
    }
}
