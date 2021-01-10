package advent2016

import scalaadventutils.Grid
import scalaadventutils.Problem

import scala.collection.mutable.ArrayBuffer

object Day8 {

    val rect = """^rect\s+(\d+)x(\d+)$""".r
    val row  = """^rotate\s+row\s+y=(\d+)\s+by\s+(\d+)$""".r
    val col  = """^rotate\s+column\s+x=(\d+)\s+by\s+(\d+)$""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day8")
        println(part1(input, 50, 6))
        println(part2(input, 50, 6).toString())
    }

    def part1(input: List[String], width: Int, height: Int) =
        runCommands(input, width, height).countOn()

    def part2(input: List[String], width: Int, height: Int) =
        runCommands(input, width, height)

    def parseLine(line: String, grid: Grid) = line match {
        case rect(w, h) => (x: Int, y: Int) =>
            if (x < w.toInt && y < h.toInt) true
            else grid.get(x, y)
        case row(r, shift) => (x: Int, y: Int) =>
            if (y == r.toInt) grid.get(x - shift.toInt, y)
            else grid.get(x, y)
        case col(c, shift) => (x: Int, y: Int) =>
            if (x == c.toInt) grid.get(x, y - shift.toInt)
            else grid.get(x, y)
    }

    private def runCommands(input: List[String], width: Int, height: Int) =
        input.foldLeft(
            new Grid(ArrayBuffer.fill(height * width)(false), width, height)
        )((next, line) => next.step(parseLine(line, next)))
}
