package advent2016

import java.security.MessageDigest

object Day5 {

    val digest = MessageDigest.getInstance("MD5")

    def main(args: Array[String]) {
        println(part1(1, Array[Char]()))
        println(part2(1, Array.fill(8)('_')))
    }

    def part1(start: Int, res: Array[Char]): String =
        if (res.size == 8) res.mkString
        else {
            val (num, ch, _) = findZeroHashChar("wtnhxymk", start)
            part1(num, res :+ ch)
        }

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

        val res = Stream.from(start + 1).dropWhile(i => {
            val hash = md5(input + i.toString)
            // Avoid string conversion of the hash to improve speed
            // A byte is the first two characters of the string, so
            // just check the 2.5 first bytes
            hash(0) != 0 || hash(1) != 0 || (hash(2) & 0xF0) != 0
        })(0)

        val chars = md5(input + res.toString)
                        .map("%02x".format(_))
                        .mkString.slice(length, length + 2)
        (res, chars(0), chars(1))
    }

    private def md5(input: String): Array[Byte] =
        digest.digest(input.getBytes)
}
