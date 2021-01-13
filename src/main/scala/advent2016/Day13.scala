package advent2016

import scalaadventutils.Dijkstra
import scalaadventutils.Grid
import scalaadventutils.WeightedUndirectedGraph

import scala.collection.mutable.ArrayBuffer

object Day13 {

    type Point = (Int, Int)
    type Graph = Map[Point, Map[Point, Int]]

    def main(args: Array[String]) {
        val g = constructGraph(constructGrid(1364, 50))
        println(part1(g))
        println(part2(g, 25, 50))
    }

    def part1(g: Graph) = getShortestPath(g, (31, 39))

    def part2(g: Graph, dimension: Int, target: Int) =
        (0 until dimension).flatMap(y => {
            (0 until dimension).map(x => {
                getShortestPath(g, (x, y))
            })
        }).filter(p => p > 0 && p <= target).size + 1

    def getShortestPath(g: Graph, target: Point) = {
        val graph = new WeightedUndirectedGraph[Point](g)
        Dijkstra.shortestPathTotalWeight(graph, Dijkstra.shortestPath(graph, (1, 1), target))
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

    def constructGrid(fav: Int, dim: Int): Grid = {
        var arr = ArrayBuffer.fill(dim * dim)(false)
        for (y <- 0 until dim) {
            for (x <- 0 until dim) {
                val z = x*x + 3*x + 2*x*y + y + y*y + fav
                val b = z.toBinaryString
                arr(y * dim + x) = b.filter(_ == '1').size % 2 == 0
            }
        }
        new Grid(arr, dim, dim)
    }
}
