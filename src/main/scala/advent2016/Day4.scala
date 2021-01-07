package advent2016

import scalaadventutils.Problem

object Day4 {
    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day4")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]): Int = input.filter( i => {
        val (map, check) = parseInput(i)
        isValid(map, check)
    }).map(getSectorID(_)).sum

    def part2(input: List[String]): Int =
        getSectorID(input.find(i => decrypt(i).contains("north")).get)

    def isValid(charMap: Array[(Char, Int)], checksum: List[Char]): Boolean = {
        return charMap.sortBy{
            case (c, i) => (-i, c)
        }.map(_._1).slice(0, 5) sameElements checksum
    }

    @throws(classOf[IllegalArgumentException])
    def parseInput(line: String): (Array[(Char, Int)], List[Char]) = {
        val parser = """^([a-z-]+)\d+\[(\w+)\]$""".r
        line match {
            case parser(name, checksum) => {
                val chars = name.replace("-", "").distinct.toCharArray
                (chars.map(x => (x, name.count(_ == x))),
                 checksum.toCharArray.toList)
            }
            case _ => throw new IllegalArgumentException
        }
    }

    @throws(classOf[IllegalArgumentException])
    def decrypt(line: String): String = {
        val parser = """^([a-z-]+)(\d+)\[\w+\]$""".r
        line match {
            case parser(name, sectorID) => {
                name.toCharArray.map(c =>
                    ((((c.toInt - 97) + sectorID.toInt) % 26) + 97).toChar
                ).mkString.replace('f', ' ').trim
            }
            case _ => throw new IllegalArgumentException
        }
    }

    private def getSectorID(line: String): Int = {
        val sectorParser = """(\d+)""".r
        return sectorParser.findFirstMatchIn(line).map(_.group(1)).get.toInt
    }
}

