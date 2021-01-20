package advent2016

import scalaadventutils.Problem

object Day23 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day23")
        println(part1(input))
        println(part2(input))
    }

    def part1(lines: List[String]) =
        new Assembunny(
            Map("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0)
        ).run(lines)("a")

    // Based on the part 2 hint about multiplication, inspecting the
    // instructions shows that the first part implements the product of
    // incremeting numbers up to the original input (12 for part 2), and the
    // second part implements multiplication of the two constants 86 and 77
    def part2(lines: List[String]) = (1 to 12).product + (86 * 77)
}
