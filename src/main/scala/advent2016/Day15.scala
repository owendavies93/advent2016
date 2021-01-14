package advent2016

import scalaadventutils.Problem

object Day15 {

    val parser = """Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day15")
        val discs = input.map(parseInput)
        println(part1(discs))
        val part2 = discs ::: List(new Disc(7, 11, 0))
        println(part1(part2))
    }

    def part1(discs: List[Disc]) =
        Stream.from(1).dropWhile(t => {
            !discs.forall(d => (d.start + (t + d.index)) % d.positions == 0)
        })(0)

    def parseInput(line: String): Disc = line match {
        case parser(index, pos, start) =>
            new Disc(index.toInt, pos.toInt, start.toInt)
    }
}

class Disc(val index: Int, val positions: Int, val start: Int)
