package advent2016

import scalaadventutils.Problem

import annotation.tailrec
import scala.collection.immutable.Queue

object Day11 {
    type FloorMap = Map[Int, (Set[Item])]

    val parser = """(\w+)(?:-compatible)? (generator|microchip)""".r

    def main(args: Array[String]) {
        println(part1())
        println(part2())
    }

    def part1() = solve(parseInput(Problem.parseInputToList("day11")))

    def part2() = solve(parseInput(Problem.parseInputToList("day11-2")))

    def getGensAndChips(items: Set[Item]) =
        (items.filter(_.isInstanceOf[Generator])
              .map(_.asInstanceOf[Generator]).toSet,
         items.filter(_.isInstanceOf[Microchip])
              .map(_.asInstanceOf[Microchip]).toSet)

    def solve(state: Floors): Int = {
        @tailrec
        def solve_(current: Floors, q: Queue[Floors], seen: Set[Floors]): Int = {
            if (current.complete()) return current.steps
            else {
                val next = current.next().filter(!seen.contains(_))
                val (current_, q_) = q.enqueue(next).dequeue
                solve_(current_, q_, seen ++ next)
            }
        }

        solve_(state, Queue[Floors](state), Set[Floors](state))
    }

    def parseInput(lines: List[String]): Floors = {
        var i = 0
        val floorMap = lines.map(l => {
            i += 1
            val items = parser.findAllMatchIn(l).map(_.toString.split(" ")).toList

            val gens  = items.filter(i => i(1) == "generator")
                             .map(i => new Generator(i(0).split("-")(0)))
                             .toSet

            val chips = items.filter(i => i(1) == "microchip")
                             .map(i => new Microchip(i(0).split("-")(0)))
                             .toSet
            (i, (gens ++ chips))
        }).toMap

        new Floors(1, floorMap, 0)
    }
}

class Floors
    ( val elevator: Int
    , val floors: Day11.FloorMap
    , val steps: Int
    ) extends Equals {

    def valid(): Boolean = {
        val v = floors.keys.toList.map(k => {
            val (gens, chips) = Day11.getGensAndChips(floors(k))
            gens.isEmpty || !chips.exists(c => !gens.exists(_.elem == c.elem))
        })
        v.forall(_ == true)
    }

    def complete(): Boolean =
        floors.keys.filter(_ < floors.keys.size).forall(k => floors(k).isEmpty)

    def next(): List[Floors] = {
        val items = floors(elevator).toList
        val combs = items.zipWithIndex.combinations(2).toList ++
                    items.zipWithIndex.combinations(1)

        List(-1, 1).flatMap(d => {
            combs.map(c => step(d, c.map(_._1)))
        }).filter(_ != None).map(_.get)
    }

    def step(change: Int, items: List[Item]): Option[Floors] = {
        val newFloor = elevator + change
        if (newFloor < 1 || newFloor > floors.keys.size) None
        else {
            val newMap: Day11.FloorMap = floors
                .updated(elevator, floors(elevator) -- items)
                .updated(newFloor, floors(newFloor) ++ items)

            val newState = new Floors(newFloor, newMap, steps + 1)
            if (newState.valid()) Some(newState) else None
        }
    }

    override def canEqual(that: Any) = that.isInstanceOf[Floors]

    override def equals(that: Any) = that match {
        case f: Floors => (this eq f) ||
                          (f.canEqual(this) && f.hashCode == hashCode
                                            && f.elevator == elevator)
        case _ => false
    }

    override def hashCode() = floors.keys.map(
        k => 31 * k * floors(k).map(_.hashCode()).sum
    ).sum

    override def toString(): String = {
        val sb = new StringBuilder()
        for (f <- floors.keys.toList.sorted.reverse) {
            sb.append("F" + f.toString)
            sb.append(if (elevator == f) " E " else " . ")
            sb.append(floors(f).mkString(" "))
            sb.append("\n")
        }

        return sb.toString()
    }
}

class Item(val elem: String) extends Equals {
    override def canEqual(that: Any) = that.isInstanceOf[Item]

    override def equals(that: Any) = that match {
        case i: Item => (this eq i) ||
                        (i.canEqual(this) && i.hashCode == hashCode
                                          && i.elem == elem
                                          && i.toString() == toString())
        case _ => false
    }

    override def hashCode() = 31 * toString().##
}

class Generator(elem: String) extends Item(elem) {
    override def toString() = elem.take(1).toUpperCase + "G"
}

class Microchip(elem: String) extends Item(elem) {
    override def toString() = elem.take(1).toUpperCase + "M"
}
