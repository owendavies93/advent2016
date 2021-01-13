package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day14Spec extends AnyFunSuite {

    test("Day 14: hasTriple") {
        var string = Day14.md5("abc18")

        assertResult('8') {
            Day14.hasTriple(string)
        }

        assert(!Day14.checkQuint(18, "abc", '8'))

        string = Day14.md5("abc39")

        assertResult('e') {
            Day14.hasTriple(string)
        }

        assert(Day14.checkQuint(39, "abc", 'e'))
    }

    test("Day 14: stretch") {
        assertResult("a107ff634856bb300138cac6568c0f24") {
            Day14.stretch("abc0")
        }
    }

    test("Day 14: part 1") {
        assertResult(22728) {
            Day14.part1("abc")
        }
    }
}
