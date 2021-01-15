package advent2016

object Day16 {

    def main(args: Array[String]) {
        println(part1())
        println(part2())
    }

    def part2() =
        Day16.checksumUntilOdd(Day16.fillDisk(35651584, "01110110101001000"))

    def part1() =
        Day16.checksumUntilOdd(Day16.fillDisk(272, "01110110101001000"))

    def checksumUntilOdd(a: String) =
        Stream.from(1).scanLeft(a)((str, i) => checksum(str))
                      .find(_.size % 2 != 0).get

    def fillDisk(size: Int, init: String) =
        Stream.from(1).scanLeft(init)((str, i) => dragonCurve(str))
                      .find(_.size > size).get.take(size)

    def dragonCurve(a: String) = a + "0" +
        (a.replace('0', 'x').replace('1', '0').replace('x', '1')).reverse

    private def checksum(a: String) = {
        val sb = new StringBuilder()

        (0 until a.size by 2).foreach(i =>
            sb.append(if (a(i) == a(i + 1)) '1' else '0')
        )

        sb.toString()
    }
}
