package advent2016

import scalaadventutils.Dijkstra
import scalaadventutils.Grid
import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

object Day24 {

    type Point = (Int, Int)
    type PointMap = Map[Int, Point]

    val numeric = """\d""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day24")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]): Int = {
        val (grid, points) = constructGrid(input)
        val g = new WeightedUndirectedGraph[Point](constructGraph(grid))
        val start = points(0)

        points.retain((k, v) => k != 0).values.toList.permutations.map(p =>
            (start :: p).sliding(2).map(s =>
                Dijkstra.shortestPathTotalWeight(
                    g, Dijkstra.shortestPath(g, s(0), s(1))
                )
            ).sum
        ).min
    }

    def part2(input: List[String]): Int = {
        val (grid, points) = constructGrid(input)
        val g = new WeightedUndirectedGraph[Point](constructGraph(grid))
        val start = points(0)

        points.retain((k, v) => k != 0).values.toList.permutations.map(p =>
            ((start :: p) :+ start).sliding(2).map(s =>
                Dijkstra.shortestPathTotalWeight(
                    g, Dijkstra.shortestPath(g, s(0), s(1))
                )
            ).sum
        ).min
    }

    def constructGraph(grid: Grid) =
        (0 until grid.height).flatMap(y => {
            (0 until grid.width).map(x => {
                val openNeighbours =
                    grid.nonDiagNeighbours(x, y)
                        .filter(n => grid.get(n._1, n._2) == true)
                ((x, y), openNeighbours.map((_, 1)).toMap)
            })
        }).toMap

    def constructGrid(input: List[String]): (Grid, PointMap) = {
        val pointMap = Map[Int, Point]()

        val height = input.size
        val width  = input(0).size

        val arr = (0 until height).flatMap(y =>
            (0 until width).map(x => {
                val ch = input(y).charAt(x)
                if (numeric.findFirstMatchIn(ch.toString) != None)
                    pointMap += (ch.toInt - 48 -> (x, y))

                ch != '#'
            })
        )
        val buf = ArrayBuffer(arr: _*)
        (new Grid(buf, width, height), pointMap)
    }

}
