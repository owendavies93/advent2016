package advent2016

import scalaadventutils.GridUtils

import org.scalatest.funsuite.AnyFunSuite

class Day8Spec extends AnyFunSuite {

    val grid = List[String](
        ".......",
        ".......",
        "......."
    )

    test("Day 8: parse and step") {
        var g1 = GridUtils.from2DCharArray(grid, '#')

        var line = "rect 3x2"
        var step = Day8.parseLine(line, g1)
        g1 = g1.step(step)

        var out = List[String](
            "###....",
            "###....",
            "......."
        )

        assert(out.mkString("\n") == g1.toString().trim())

        line = "rotate column x=1 by 1"
        step = Day8.parseLine(line, g1)
        g1 = g1.step(step)

        out = List[String](
            "#.#....",
            "###....",
            ".#....."
        )

        println(out.mkString("\n"))
        println()
        println(g1.toString())

        assert(out.mkString("\n") == g1.toString().trim())

        line = "rotate row y=0 by 4"
        step = Day8.parseLine(line, g1)
        g1 = g1.step(step)

        out = List[String](
            "....#.#",
            "###....",
            ".#....."
        )

        assert(out.mkString("\n") == g1.toString().trim())

        line = "rotate column x=1 by 1"
        step = Day8.parseLine(line, g1)
        g1 = g1.step(step)

        out = List[String](
            ".#..#.#",
            "#.#....",
            ".#....."
        )

        assert(out.mkString("\n") == g1.toString().trim())
    }
}
