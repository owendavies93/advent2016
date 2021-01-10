package advent2016

import scala.annotation.tailrec
import scalaadventutils.Problem

object Day9 {
    val marker = """^\((\d+)x(\d+)\).*""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToString("day9")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: String) = getLength(input)

    def part2(input: String) = getRecursiveLength(input)

    def getLength(input: String): Int = {
        @tailrec
        def getLength_(str: String, len: Int): Int = str match {
            case "" => len
            case marker(chars, mult) => {
                val l = chars.size + mult.size + 3
                val total = chars.toInt * mult.toInt
                getLength_(str.drop(chars.toInt + l), len + total)
            }
            case _ => getLength_(str.drop(1), len + 1)
        }

        getLength_(input, 0)
    }

    def getRecursiveLength(input: String): Long = {
        def getRecursiveLength_(str: String, len: Long): Long = str match {
            case "" => len
            case marker(chars, mult) => {
                var next = str.drop(chars.size + mult.size + 3)
                val total = getRecursiveLength_(next.take(chars.toInt), 0) *
                            mult.toInt
                next = next.drop(chars.toInt)
                getRecursiveLength_(next, len + total)
            }
            case _ => getRecursiveLength_(str.drop(1), len + 1)
        }

        getRecursiveLength_(input, 0)
    }
}
