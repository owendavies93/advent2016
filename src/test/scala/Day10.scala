package advent2016

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day10Spec extends AnyFunSuite {
    test("Day 10: parsing and stepping") {
        var string = "value 5 goes to bot 2"
        var state = Map[String, Set[Int]]()
        state = Day10.parseCommand(state, string)

        assertResult(Set(5)) {
            state("bot 2")
        }

        state = Map("bot 2" -> Set(5, 1))
        string = "bot 2 gives low to bot 1 and high to bot 0"
        state = Day10.parseCommand(state, string)

        assertResult(Set()) {
            state("bot 2")
        }

        assertResult(Set(5)) {
            state("bot 0")
        }

        assertResult(Set(1)) {
            state("bot 1")
        }

        state = Map("bot 1" -> Set(5, 1))
        string = "bot 1 gives low to output 1 and high to bot 0"
        state = Day10.parseCommand(state, string)

        assertResult(Set()) {
            state("bot 1")
        }

        assertResult(Set(5)) {
            state("bot 0")
        }

        assertResult(Set(1)) {
            state("output 1")
        }
    }

    test("Day 10: findComparingBot") {
        val input = Problem.parseInputToList("day10-test")

        assertResult("bot 2") {
            Day10.findComparingBot(input, Map[String, Set[Int]](), Set(2, 5))
        }

        assertResult(30) {
            Day10.findOutputs(input, Map[String, Set[Int]]())
        }
    }
}
