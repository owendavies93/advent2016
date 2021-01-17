package advent2016

import scalaadventutils.CellulaAutomata
import scalaadventutils.Problem

import scala.collection.mutable.ArrayBuffer

object Day18 {

    def main(args: Array[String]) {
        val start = Problem.parseInputToString("day18")
        val tr = fromString(start)

        println(tr.grid.size * 40 - getOn(tr, 40))
        println(tr.grid.size * 400000 - getOn(tr, 400000))
    }

    def getOn(tr: TileRow, rows: Int): Int =
        (1 until rows).foldLeft(tr)((next, i) => next.getNext()).countOn()

    private def fromString(input: String) = {
        val b = ArrayBuffer(input.map(_ == '^'): _*)
        new TileRow(b, b.size, 1)
    }
}

class TileRow
    ( grid: ArrayBuffer[Boolean]
    , width: Int
    , height: Int
    ) extends CellulaAutomata(grid, width, height) {

    def getNext(): TileRow = {

        def stepFn(x: Int, y: Int): Boolean = {
            val ns = neighbours(x, y).map(n => get(n._1, n._2))
            ns(0) != ns(2)
        }

        step(stepFn)
    }

    override def neighbours(x: Int, y: Int) =
        (-1 to 1).map(n => (x + n, y - 1)).toList

    override def step(stepFn: (Int, Int) => Boolean): TileRow = {
        val nextGrid = grid ++ ArrayBuffer(
            (0 until width).map(x => stepFn(x, height)): _*
        )
        new TileRow(nextGrid, width, height + 1)
    }
}
