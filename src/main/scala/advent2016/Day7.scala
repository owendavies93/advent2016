package advent2016

import scalaadventutils.Problem

object Day7 {
    val parser = """\[[^\]]+\]"""
    val abba   = """([a-z])((?!\1).)\2\1""".r
    val aba    = """(?=(([a-z])(?!\2)[a-z]\2))""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day7")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]) = input.filter(supportsTLS).size

    def part2(input: List[String]) = input.filter(supportsSSL).size

    def supportsTLS(line: String): Boolean = {
        val (hypers, nots) = parseLine(line)
        hypers.forall(!hasABBA(_)) && nots.exists(hasABBA)
    }

    def supportsSSL(line: String): Boolean = {
        val (hypers, nots) = parseLine(line)
        val abs = getABAs(nots)
        val babs = getABAs(hypers)
        abs.size > 0 && babs.map(ABAtoBAB).toSet.intersect(abs.toSet).size > 0
    }

    def parseLine(line: String): (List[String], List[String]) =
        ( parser.r.findAllMatchIn(line).map(_.toString).toList
        , line.split(parser).toList)

    private def hasABBA(s: String) = abba.findAllIn(s).size > 0

    private def getABAs(ss: List[String]) =
        ss.flatMap(aba.findAllMatchIn(_).map(_.group(1)))

    private def ABAtoBAB(aba: String) = aba(1).toString + aba(0) + aba(1)
}
