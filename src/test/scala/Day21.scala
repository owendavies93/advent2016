package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day21Spec extends AnyFunSuite {

    test("Day 21: parseLine") {
        val lines = Problem.parseInputToList("day21-test")

        val res = lines.foldLeft("abcde")((acc, next) => {
            Day21.parseLine(next, acc)
        })

        assertResult("decab") {
            res
        }
    }
}
