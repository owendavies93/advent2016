package advent2016

import scalaadventutils.Problem
import collection.mutable.ListBuffer

object Day1 {
    import advent2016.Direction._

    val dirMap = Map[Direction.Value, Map[Char, Direction.Value]](
        N -> Map('R' -> E, 'L' -> W),
        E -> Map('R' -> S, 'L' -> N),
        S -> Map('R' -> W, 'L' -> E),
        W -> Map('R' -> N, 'L' -> S)
    )

    val transform = Map[Direction.Value, (Int, Int)](
        N -> (1 , 0),
        E -> (0 , 1),
        S -> (-1, 0),
        W -> (0 ,-1)
    )

    def main(args: Array[String]) {
        val dirs = Problem.parseInputLineToList("day1", ", ")
                          .map(d => (d(0), d.substring(1).toInt))
        println(part1(dirs))
        println(part2(dirs))
    }

    def part1(dirs: List[(Char, Int)]): Int = {
        val start = new Location(0, 0, N)
        val end = dirs.foldLeft(start)(
            (loc, dir) => nextLocation(loc, dir._1, dir._2)
        )
        return Math.abs(end.x) + Math.abs(end.y)
    }

    def part2(dirs: List[(Char, Int)]): Int = {
        val start = new Location(0, 0, N)
        val points = ListBuffer[Location]()

        val end = dirs.foldLeft(start)((loc, dir) => {
            val l = nextLocation(loc, dir._1, dir._2)
            points += loc
            points ++= pointsBetweenPoints(loc, l)
            l
        })

        val firstRepeatIndex = points.zipWithIndex.map {
            case (el, i) => (0 until i).find(x => points(x).equals(el))
        }.filter(_ != None)(0).get
        val repeatPoint = points(firstRepeatIndex)

        return Math.abs(repeatPoint.x) + Math.abs(repeatPoint.y)
    }

    def nextLocation(prev: Location, turn: Char, distance: Int): Location = {
        val newDir = dirMap.get(prev.dir).get(turn)
        val (x, y) = transform.getOrElse(newDir, (0, 0))
        new Location(prev.x + (distance * x), prev.y + (distance * y), newDir)
    }

    def pointsBetweenPoints(prev: Location, next: Location): List[Location] = {
        if (prev.x == next.x) {
            val range = if (prev.y < next.y) (prev.y + 1 until next.y)
                        else (prev.y - 1 until next.y by -1)

            range.map( y => new Location(prev.x, y, prev.dir) ).toList
        } else {
            val range = if (prev.x < next.x) (prev.x + 1 until next.x)
                        else (prev.x - 1 until next.x by -1)

            range.map( x => new Location(x, prev.y, prev.dir) ).toList
        }
    }
}

object Direction extends Enumeration {
    val N, E, S, W = Value
}

class Location(val x: Int, val y: Int, val dir: Direction.Value) {
    override def equals(that: Any): Boolean = that match {
        case that: Location => that.x == x && that.y == y
        case _              => false
    }

    override def toString(): String = x + ", " + y
}

