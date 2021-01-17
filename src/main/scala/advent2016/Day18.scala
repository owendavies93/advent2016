package advent2016

import scalaadventutils.CellulaAutomata
import scalaadventutils.Problem

import annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day18 {

    def main(args: Array[String]) {
        val start = Problem.parseInputToString("day18")
        val ca = fromString(start)

        println(ca.grid.size * 40 - getOn(ca, 40))
        println(ca.grid.size * 400000 - getOn(ca, 400000))
    }

    def getOn(ca: CellulaAutomata, rows: Int): Int = {
        @tailrec
        def getOn_(c: CellulaAutomata, i: Int, total: Int): Int = {
            if (i == rows) total
            else {
                def stepFn(x: Int, y: Int): Boolean = {
                    val ns = (-1 to 1).map(n => (x + n, y)).map(n => {
                        c.get(n._1, n._2)
                    })
                    ns(0) != ns(2)
                }
                getOn_(c.step(stepFn), i + 1, total + c.countOn)
            }
        }

        getOn_(ca, 0, 0)
    }

    private def fromString(input: String): CellulaAutomata = {
        val b = ArrayBuffer(input.map(_ == '^'): _*)
        new CellulaAutomata(b, b.size, 1)
    }
}

