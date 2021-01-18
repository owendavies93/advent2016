package advent2016

import scalaadventutils.Problem

object Day20 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToListOfLongArray("day20", "-")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(lines: List[Array[Long]]) = getLowest(mergeSegments(lines), 0L)

    def part2(lines: List[Array[Long]]) = getGapSum(mergeSegments(lines))

    def getGapSum(segments: List[(Long, Long)]) =
        segments.flatten {
            case (a, b) => List(a, b)
        }.drop(1).init.grouped(2).map(y => y(1) - y(0) - 1).sum

    def getLowest(segments: List[(Long, Long)], min: Long) =
        if (segments(0)._1 <= min) segments(0)._2 + 1 else min

    def mergeSegments(lines: List[Array[Long]]): List[(Long, Long)] =
        lines.sortBy(_(0)).foldLeft( List[(Long, Long)]() ) {
            case (List(), i)                         => List((i(0), i(1)))
            case (acc, i) if acc.last._2 >= i(1)     => acc
            case (acc, i) if acc.last._2 >= i(0) - 1 => acc.init :+ (acc.last._1, i(1))
            case (acc, i)                            => acc :+ (i(0), i(1))
        }.toList
}

