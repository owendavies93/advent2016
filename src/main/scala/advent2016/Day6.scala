package advent2016

import scalaadventutils.Problem

object Day6 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day6")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]): String =
        (0 until input(0).size).map(i =>
            input.map(_(i)).groupBy(identity).maxBy(_._2.size)._1
        ).mkString

    def part2(input: List[String]): String =
        (0 until input(0).size).map(i =>
            input.map(_(i)).groupBy(identity).minBy(_._2.size)._1
        ).mkString
}
