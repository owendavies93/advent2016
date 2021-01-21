package advent2016

import scalaadventutils.Problem

object Day25 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day25")
        println(part1(input))
    }

    def part1(lines: List[String]) =
        new Assembunny(
            Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
        ).runWithOutputCheck(lines)

}
