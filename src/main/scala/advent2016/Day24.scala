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
    val cache = Map[(Point, Point), Int]()

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day24")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]) = getMinPath(input, false)

    def part2(input: List[String]) = getMinPath(input, true)

    def constructGraph(grid: Grid) =
        (0 until grid.height).flatMap(y => {
            (0 until grid.width).map(x => {
                (
                    (x, y),
                    grid.nonDiagNeighbours(x, y)
                        .filter(n => grid.get(n._1, n._2) == true)
                        .map((_, 1)).toMap
                )
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

    private def getMinPath(input: List[String], goHome: Boolean): Int = {
        cache.clear()
        val (grid, points) = constructGrid(input)
        val g = new WeightedUndirectedGraph[Point](constructGraph(grid))
        val start = points(0)

        points.retain((k, v) => k != 0).values.toList.permutations.map(p => {
            val route = (if (goHome) ((start :: p) :+ start) else (start :: p))
            route.sliding(2).map(s => shortestPath(s(0), s(1), g)).sum
        }).min
    }

    private def shortestPath
        ( from: Point
        , to: Point
        , g: WeightedUndirectedGraph[Point]): Int = {

        if (cache contains (from, to)) {
            cache((from, to))
        } else if (cache contains (to, from)) {
            cache((to, from))
        } else {
            val path = Dijkstra.shortestPathTotalWeight(
                g, Dijkstra.shortestPath(g, from, to)
            )

            cache += ((from, to) -> path)
            cache += ((to, from) -> path)
            path
        }
    }
}
