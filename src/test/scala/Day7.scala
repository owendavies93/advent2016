package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day7Spec extends AnyFunSuite {

    test("Day 7: parsing") {
        val string = "abba[mnop]qrst"
        assertResult((List("[mnop]"), List("abba", "qrst"))) {
            Day7.parseLine(string)
        }
    }

    test("Day 7: validation") {
        var string = "abba[mnop]qrst"

        assert(Day7.supportsTLS(string))

        string = "abcd[bddb]xyyx"

        assert(!Day7.supportsTLS(string))

        string = "aaaa[qwer]tyui"

        assert(!Day7.supportsTLS(string))

        string = "ioxxoj[asdfgh]zxcvbn"

        assert(Day7.supportsTLS(string))

        string = "aba[bab]xyz"

        assert(Day7.supportsSSL(string))

        string = "xyx[xyx]xyx"

        assert(!Day7.supportsSSL(string))

        string = "aaa[kek]eke"

        assert(Day7.supportsSSL(string))

        string = "zazbz[bzb]cdb"

        assert(Day7.supportsSSL(string))
    }
}
