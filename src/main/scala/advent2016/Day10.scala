package advent2016

import scalaadventutils.Problem

object Day10 {
    val goes  = """^value (\d+) goes to (bot \d+)$""".r
    val gives =
        """^(bot \d+) gives low to ([\w\s]+) and high to ([\w\s]+)$""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day10")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]) =
        findComparingBot(input, Map[String, Set[Int]](), Set(17, 61))

    def part2(input: List[String]) =
        findOutputs(input, Map[String, Set[Int]]())

    def findComparingBot
        ( input: List[String]
        , state: Map[String, Set[Int]]
        , target: Set[Int])
        : String = {

        val states = input.scanLeft(state)(parseCommand)
        states.flatMap(x => x.find(_._2 == target).map(_._1)).headOption
              .getOrElse(findComparingBot(input, states.last, target))
    }

    def findOutputs(input: List[String], state: Map[String, Set[Int]]): Int = {
        val states = input.scanLeft(state)(parseCommand)
        val solution = states.find(state => {
            state.contains("output 0") && state.contains("output 1") &&
            state.contains("output 2")
        }).headOption

        if (solution != None)
            (solution.get("output 0") ++
             solution.get("output 1") ++
             solution.get("output 2")
            ).product
        else findOutputs(input, states.last)
    }

    def parseCommand
        ( state: Map[String, Set[Int]]
        , command: String)
        : Map[String, Set[Int]] = command match {

        case goes(value, bot) =>
            state.updated(bot, getOrEmpty(state, bot) + value.toInt)
        case gives(bot, low, high) if (getOrEmpty(state, bot).size == 2) =>
            state.updated(low,  getOrEmpty(state, low)  + state(bot).min)
                 .updated(high, getOrEmpty(state, high) + state(bot).max)
                 .updated(bot,  state(bot).empty)
        case _ => state
    }

    private def getOrEmpty(state: Map[String, Set[Int]], key: String) =
        state.getOrElse(key, Set[Int]())
}
