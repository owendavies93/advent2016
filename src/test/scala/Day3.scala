package advent2016

import org.scalatest.funsuite.AnyFunSuite

class Day3Spec extends AnyFunSuite {

    test("Day 3: isTriangle") {
        assert(!Day3.isTriangle(List(2)))
        assert(!Day3.isTriangle(List(5, 10, 25)))
        assert( Day3.isTriangle(List(5, 10, 12)))
        assert(!Day3.isTriangle(List(5, 25, 5)))
        assert( Day3.isTriangle(List(5, 10, 10)))
        assert( Day3.isTriangle(List(15, 10, 10)))
    }
}
