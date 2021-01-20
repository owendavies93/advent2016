package advent2016

import scalaadventutils.Problem

object Day12 {
    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day12")
        println(part1(input))
        println(part2(input))
    }

    def part1(lines: List[String]) =
        new Assembunny(
            Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
        ).run(lines)("a")

    def part2(lines: List[String]) =
        new Assembunny(
            Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)
        ).run(lines)("a")
}
