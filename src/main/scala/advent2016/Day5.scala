package advent2016

import java.security.MessageDigest

object Day5 {

    val digest = MessageDigest.getInstance("MD5")

    def main(args: Array[String]) {
        println(part2())
    }

    def part1(): String = {
        var start = 1

        return (1 to 8).map(i => {
            val (res, ch, _) = findZeroHashChar("wtnhxymk", start)
            start = res
            ch
        }).mkString
    }

    def part2(): String = {
        var start = 1
        var out = List.fill(8)('_').mkString

        // So imperative yuck!
        while (out.filter(_ == '_').size > 0) {
            val (res, index, ch) = findZeroHashChar("wtnhxymk", start)
            val i = index.toInt - 48
            if (i < 8 && out.charAt(i) == '_') {
                out = out.substring(0, i) + ch + out.substring(i + 1)
            }
            start = res
        }

        return out
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
