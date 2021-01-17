package advent2016

import scala.math.pow

object Day19 {

    def main(args: Array[String]) {
        println(part1())
        println(part2(3005290))
    }

    def part1() = josephus(3005290)

    /*
        Patern worked out by hand:
        * If the input is a power of 3, the input is the winner
        * Between powers of 3:
            * the winner increments for the first (last power of 3) numbers
                This is the same as subtracting the power of 3 from the input
            * then increases by 2 until the next power of 3 is reached
                This is the same as doubling the input and subtracting
                the power of 3 3 times
    */
    def part2(i: Int) = {
        val highestPower = pow(
            3, Stream.from(1).dropWhile(pow(3, _) < i)(0) - 1
        ).toInt

        if (i == highestPower) {
            i
        } else if (i - highestPower <= highestPower) {
            i - highestPower
        } else {
            2 * i - 3 * highestPower
        }
    }

    // This is a simplified case of the Josephus problem
    def josephus(i: Int) = 1 + 2 * Integer.parseInt(i.toBinaryString.drop(1), 2)

}
