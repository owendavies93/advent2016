package advent2016

import scalaadventutils.Hashing

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

object Day17 {
    import advent2016.Dir._

    val transform = Map[Dir.Value, (Int, Int)](
        U -> (0 ,-1),
        D -> (0 , 1),
        L -> (-1, 0),
        R -> (1 , 0)
    )

    def main(args: Array[String]) {
        println(part1())
        println(part2())
    }

    def part1() = getPath(new Point(0, 0), new Point(3, 3), "rrrbmfta")(0)

    def part2() = getPath(new Point(0, 0), new Point(3, 3), "rrrbmfta").last.size

    def getPath
        ( p: Point
        , target: Point
        , start: String)
        : List[String] = {

        val q = Queue[(Point, List[Point], List[Dir.Value])]((p, List(p), List()))
        val res = ListBuffer[String]()

        while (!q.isEmpty) {
            val (p_, path, dirs) = q.dequeue

            // Grim
            def update(n: (Point, Dir.Value)): Unit = {
                if (n._1 == target) {
                    res += (dirs :+ n._2).mkString
                } else {
                    q += ((n._1, path :+ n._1, dirs :+ n._2))
                }
            }

            for (n <- getValidMoves(p_, start + dirs.mkString)) update(n)
        }

        res.toList
    }

    def getValidMoves(p: Point, pass: String): List[(Point, Dir.Value)] =
        getAllowedDirs(Hashing.md5AsString(pass))
            .map(d => (p.transform(transform(d)), d))
            .filter(x => inBounds(x._1)).toList

    def getAllowedDirs(hash: String): List[Dir.Value] =
        hash.take(4).zipWithIndex.filter(ch => ch._1 >= 'b' && ch._1 <= 'f')
                                 .map(ch => Dir(ch._2)).toList

    private def inBounds(p: Point) =
        p.x >= 0 && p.x <= 3 && p.y >= 0 && p.y <= 3
}

class Point(val x: Int, val y: Int) extends Equals {
    def transform(t: (Int, Int)) = new Point(x + t._1, y + t._2)

    override def canEqual(that: Any) = that.isInstanceOf[Point]

    override def equals(that: Any) = that match {
        case p: Point => p.canEqual(this) && p.x == x && p.y == y
        case _ => false
    }

    override def toString() = "(" + x + ", " + y + ")"
}

object Dir extends Enumeration {
    val U, D, L, R = Value
}
