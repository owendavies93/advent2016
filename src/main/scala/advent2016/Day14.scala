package advent2016

import scalaadventutils.Hashing

import annotation.tailrec
import scala.collection.mutable.Map

object Day14 {

    val triple = """(\w)\1\1""".r
    val cache  = Map[String, String]()
    val sCache = Map[String, String]()

    def main(args: Array[String]) {
        println(part1("qzyelonm"))
        println(part1("qzyelonm", true))
    }

    def part1(base: String, isStretch: Boolean = false) = {
        @tailrec
        def getNext(i: Int, count: Int): Int = {
            if (count == 64) i
            else
                getNext(
                    Stream.from(i + 1).dropWhile(n => {
                        val c = hasTriple(md5(base + n.toString, isStretch))
                        c == '-' || !checkQuint(n, base, c, isStretch)
                    })(0), count + 1
                )
        }

        getNext(0, 0)
    }

    def checkQuint
        ( start: Int
        , base: String
        , target: Char
        , isStretch: Boolean = false)
        : Boolean = {

        val substr = List.fill(5)(target).mkString

        @tailrec
        def check(i: Int): Boolean = {
            if (i >= start + 1000) {
                return false
            }

            if (md5(base + i.toString, isStretch).contains(substr)) true
            else check(i + 1)
        }
        check(start + 1)
    }

    def hasTriple(x: String) = triple.findFirstMatchIn(x) match {
        case Some(i) => i.toString().charAt(0)
        case None    => '-'
    }

    def md5(x: String, isStretch: Boolean) =
        if (isStretch) sCache.getOrElseUpdate(x, Hashing.md5Multi(x, 2017))
        else cache.getOrElseUpdate(x, Hashing.md5AsString(x))
}
