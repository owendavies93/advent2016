package advent2016

import scalaadventutils.Problem

object Day2 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day2")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(input: List[String]) =
        input.scanLeft(5)((next, line) => tracePath(line, next))
             .mkString.substring(1)

    def part2(input: List[String]) =
        input.scanLeft('5')((next, line) => tracePath2(line, next))
             .mkString.substring(1)

    def tracePath(path: String, start: Int): Int =
        path.toCharArray.foldLeft(start)((next, dir) => transform(dir, next))

    def tracePath2(path: String, start: Char): Char =
        path.toCharArray.foldLeft(start) {(next, dir) =>
            val t = transform2(dir, next)
            if (t != None) {
                t.get
            } else {
                next
            }
        }

    private def transform(dir: Char, start: Int): Int = dir match {
        case 'U' => if (start > 3) start - 3 else start
        case 'D' => if (start < 7) start + 3 else start
        case 'R' => if (start % 3 != 0) start + 1 else start
        case 'L' => if (start % 3 != 1) start - 1 else start
    }

    private def transform2(dir: Char, start: Char): Option[Char]
        = part2MoveMap(start).get(dir)

    // Can't think of a neater way to do this like part 1
    val part2MoveMap = Map(
        '1' -> Map( 'D' -> '3' ),
        '2' -> Map( 'R' -> '3', 'D' -> '6' ),
        '3' -> Map( 'U' -> '1', 'D' -> '7', 'L' -> '2', 'R' -> '4' ),
        '4' -> Map( 'D' -> '8', 'L' -> '3' ),
        '5' -> Map( 'R' -> '6' ),
        '6' -> Map( 'U' -> '2', 'D' -> 'A', 'L' -> '5', 'R' -> '7' ),
        '7' -> Map( 'U' -> '3', 'D' -> 'B', 'L' -> '6', 'R' -> '8' ),
        '8' -> Map( 'U' -> '4', 'D' -> 'C', 'L' -> '7', 'R' -> '9' ),
        '9' -> Map( 'L' -> '8' ),
        'A' -> Map( 'U' -> '6', 'R' -> 'B' ),
        'B' -> Map( 'U' -> '7', 'D' -> 'D', 'L' -> 'A', 'R' -> 'C' ),
        'C' -> Map( 'U' -> '8', 'L' -> 'B' ),
        'D' -> Map( 'U' -> 'B' )
    )

}
