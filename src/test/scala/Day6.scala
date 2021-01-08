package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day6Spec extends AnyFunSuite {

    val input = List[String](
        "eedadn",
        "drvtee",
        "eandsr",
        "raavrd",
        "atevrs",
        "tsrnev",
        "sdttsa",
        "rasrtv",
        "nssdts",
        "ntnada",
        "svetve",
        "tesnvt",
        "vntsnd",
        "vrdear",
        "dvrsen",
        "enarar"
    )

    test("Day 6: part 1") {
        assertResult("easter") {
            Day6.part1(input)
        }
    }

    test("Day 6: part 2") {
        assertResult("advent") {
            Day6.part2(input)
        }
    }
}
