package advent2016

import scalaadventutils.Hashing

import annotation.tailrec

object Day5 {

    def main(args: Array[String]) {
        println(part1(1, Array[Char]()))
        println(part2(1, Array.fill(8)('_')))
    }

    @tailrec
    def part1(start: Int, res: Array[Char]): String =
        if (res.size == 8) res.mkString
        else {
            val (num, ch, _) = findZeroHashChar("wtnhxymk", start)
            part1(num, res :+ ch)
        }

    @tailrec
    def part2(start: Int, res: Array[Char]): String =
        if (res.filter(_ == '_').size == 0) return res.mkString
        else {
            val (num, index, ch) = findZeroHashChar("wtnhxymk", start)
            val i = index.toInt - 48
            if (i < 8 && res.charAt(i) == '_') {
                res(i) = ch
            }
            part2(num, res)
        }

    def findZeroHashChar
        ( input: String
        , start: Int
        , length: Int = 5): (Int, Char, Char)  = {

        val res = Hashing.findHashWithLeadingChars(input, start, length, 0)
        val chars = Hashing.md5AsString(input + res.toString)
                           .slice(length, length + 2)
        (res, chars(0), chars(1))
    }
}
