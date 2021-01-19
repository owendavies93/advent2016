package advent2016

import scalaadventutils.Problem

object Day22 {

    val parser = """/dev/grid/node-x(\d+)-y(\d+)\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%""".r

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day22")
        println(part1(lines))

        val nodes = lines.map(parseLine)
        printGrid(nodes)
    }

    // There's only one empty node so just count the nodes
    // that fit into that node
    // This doesn't work for inputs
    def part1(lines: List[String]): Int = {
        val nodes = lines.map(parseLine)
        val empty = nodes.find(n => n.used == 0).get
        nodes.filter(n => n != empty && n.used <= empty.avail).size
    }

    // Solve by hand by printing the grid
    // Avoid the large nodes (found by inspecting the input)
    // move the empty node to the goal node, and perform the swap
    // operation (maxX - 1) times until we get the goal data
    // to our node
    def printGrid(nodes: List[Node]) {
        val nodeMap = nodes.map(n => ((n.x, n.y) -> n)).toMap

        val maxX = nodes.map(_.x).max
        val maxY = nodes.map(_.y).max

        println(maxX)
        println(maxY)

        for (y <- 0 to maxY) {
            val out = (0 to maxX).map(x => {
                val n = nodeMap(x, y)
                if (n.x == maxX && n.y == 0) 'G' else n
            }).mkString
            println(out)
        }
    }

    def parseLine(line: String): Node = line match {
        case parser(x, y, used, avail) =>
            new Node(x.toInt, y.toInt, used.toInt, avail.toInt)
    }
}

class Node
    ( val x: Int
    , val y: Int
    , val used: Int
    , val avail: Int) extends Equals {

    def isEmpty = used == 0

    def size = used + avail

    override def canEqual(that: Any) = that.isInstanceOf[Node]

    override def equals(that: Any) = that match {
        case n: Node => n.canEqual(this) && n.x == x && n.y == y
        case _       => false
    }

    override def toString() =
        if (used == 0) { "_" }
        else if (size > 150) { "#" }
        else { "." }
}
