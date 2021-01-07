package advent2016

import scalaadventutils.Problem

object Day3 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToListOfIntArray("day3", " ")
                           .map(_.toList)

        println(part1(lines))

        val vertical = (0 to 2).map(i => {
            lines.flatten.zipWithIndex.filter(_._2 % 3 == i)
        }).flatten.map(_._1).toList.grouped(3).toList

        println(part1(vertical))
    }

    def part1(lines: List[List[Int]]): Int = lines.filter(isTriangle).size

    def isTriangle(sides: List[Int]): Boolean = {
        if (sides.size != 3) return false

        val zip = sides.zipWithIndex
        return zip.combinations(2).forall(t => {
            val other = zip.find(!t.contains(_)).get
            t.map(_._1).sum > other._1
        })
    }
}
