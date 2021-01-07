package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day4Spec extends AnyFunSuite {

    test("Day 4: parsing") {
        val string = "aaaaa-bbb-z-y-x-123[abxyz]"

        val parsed = Day4.parseInput(string)

        assertResult(Array(('a', 5), ('b', 3), ('z', 1), ('y', 1), ('x', 1))) {
            parsed._1
        }

        assertResult(List('a', 'b', 'x', 'y', 'z')) {
            parsed._2
        }
    }

    test("Day 4: isValid") {
        var string = "aaaaa-bbb-z-y-x-123[abxyz]"
        var parse = Day4.parseInput(string)
        var (map, check) = parse

        assert(Day4.isValid(map, check))

        string = "a-b-c-d-e-f-g-h-987[abcde]"
        parse = Day4.parseInput(string)
        map = parse._1
        check = parse._2

        assert(Day4.isValid(map, check))

        string = "not-a-real-room-404[oarel]"
        parse = Day4.parseInput(string)
        map = parse._1
        check = parse._2

        assert(Day4.isValid(map, check))

        string = "totally-real-room-200[decoy]"
        parse = Day4.parseInput(string)
        map = parse._1
        check = parse._2

        assert(!Day4.isValid(map, check))
    }

    test("Day 4: decrypt") {
        val string = "qzmt-zixmtkozy-ivhz-343[asdas]"

        assertResult("very encrypted name") {
            Day4.decrypt(string)
        }
    }
}
