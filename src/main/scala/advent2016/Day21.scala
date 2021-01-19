package advent2016

import scalaadventutils.Problem

object Day21 {

    val swapP = """swap position (\d) with position (\d)""".r
    val swapL = """swap letter (\w) with letter (\w)""".r
    val rot   = """rotate (left|right) (\d+) steps?""".r
    val rotP  = """rotate based on position of letter (\w)""".r
    val rev   = """reverse positions (\d) through (\d)""".r
    val move  = """move position (\d) to position (\d)""".r

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day21")
        println(part1(lines, "abcdefgh"))
        println(part2(lines, "fbgdceah"))
    }

    def part1(lines: List[String], start: String) =
        lines.foldLeft(start)((acc, next) => parseLine(next, acc))

    def part2(lines: List[String], target: String) =
        target.permutations.dropWhile(perm =>
            lines.foldLeft(perm)((acc, next) => parseLine(next, acc)) != target
        ).take(1).toList(0)

    def parseLine(line: String, s: String): String = line match {
        case swapP(x, y) =>
            s.updated(y.toInt, s(x.toInt))
             .updated(x.toInt, s(y.toInt)).mkString
        case swapL(x, y) => {
            val a = s.indexOf(x)
            val b = s.indexOf(y)
            s.updated(a, y).updated(b, x).mkString
        }
        case rot(dir, steps) =>
            if (dir == "left") s.drop(steps.toInt) ++ s.take(steps.toInt)
            else s.drop(s.size - steps.toInt) ++ s.take(s.size - steps.toInt)
        case rotP(x) => {
            val i = s.indexOf(x)
            val steps = 1 + i + (if (i >= 4) 1 else 0)
            s.drop(s.size - (steps.toInt % s.size)) ++
            s.take(s.size - (steps.toInt % s.size))
        }
        case rev(x, y) => {
            val a = x.toInt
            val b = y.toInt
            s.substring(0, a) + s.substring(a, b + 1).reverse +
            s.substring(b + 1)
        }
        case move(x, y) => {
            val a = x.toInt
            val b = y.toInt
            if (a < b) {
                s.substring(0, a) + s.substring(a + 1, b + 1) + s(a) +
                s.substring(b + 1)
            } else if (a > b) {
                s.substring(0, b) + s(a) + s.substring(b, a) +
                s.substring(a + 1)
            } else {
                s
            }
        }
    }
}
