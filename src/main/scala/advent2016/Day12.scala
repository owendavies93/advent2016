package advent2016

import scalaadventutils.Problem

import annotation.tailrec

object Day12 {

    type Machine = Map[String, Int]

    val numeric = """\d+""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day12")
        println(part1(input))
        println(part2(input))
    }

    def part1(lines: List[String]) = run(lines)("a")

    def part2(lines: List[String]) = run(lines, true)("a")

    def parseLine(line: String): List[String] = line.split(" ").toList

    def run(lines: List[String], part2: Boolean = false): Machine = {
        @tailrec
        def run_(regs: Machine, ptr: Int): Machine = {
            if (ptr < 0 || ptr >= lines.size) regs
            else {
                val (regs_, ptr_) = step(regs, ptr, parseLine(lines(ptr)))
                run_(regs_, ptr_)
            }
        }

        val m = if (part2) Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0)
                else       Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0)
        run_(m, 0)
    }

    private def step(regs: Machine, ptr: Int, comm: List[String]): (Machine, Int) = {
        val cmd :: args = comm

        cmd match {
            case "cpy" => args(0) match {
                case numeric(_*) => (regs.updated(args(1), args(0).toInt), ptr + 1)
                case _           => (regs.updated(args(1), regs(args(0))), ptr + 1)
            }
            case "inc" => (regs.updated(args(0), regs(args(0)) + 1), ptr + 1)
            case "dec" => (regs.updated(args(0), regs(args(0)) - 1), ptr + 1)
            case "jnz" => args(0) match {
                case numeric(_*) => if (args(0).toInt != 0) (regs, ptr + args(1).toInt)
                                    else (regs, ptr + 1)
                case _           => if (regs(args(0)) != 0) (regs, ptr + args(1).toInt)
                                    else (regs, ptr + 1)
            }
        }
    }
}
