package advent2016

import scalaadventutils.Problem

import scala.collection.mutable.Queue

object Day11 {
    type FloorMap = Map[Int, (Set[Generator], Set[Microchip])]

    val parser = """(\w+)(?:-compatible)? (generator|microchip)""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day11")
        println(solve(parseInput(input)))

        val input2 = Problem.parseInputToList("day11-2")
        println(solve(parseInput(input2)))
    }

    def solve(state: Floors): Int = {
        val q = Queue[Floors](state)
        val seen = collection.mutable.Set[Floors](state)

        while (!q.isEmpty) {
            val current = q.dequeue()

            if (current.complete()) return current.steps
            else {
                val next = current.next()

                for (n <- next) {
                    if (!seen.contains(n)) {
                        q += n
                        seen += n
                    }
                }
            }
        }

        return -1
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
            (i, (gens, chips))
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
            val (gens, chips) = floors(k)
            gens.isEmpty || !chips.exists(c => !gens.exists(_.elem == c.elem))
        })
        v.forall(_ == true)
    }

    def complete(): Boolean =
        floors.keys.filter(_ < floors.keys.size)
              .forall(k => (floors(k)._1 ++ floors(k)._2).isEmpty)

    def next(): List[Floors] = {
        val items = floors(elevator)._1.toList ++ floors(elevator)._2
        val combs = items.zipWithIndex.combinations(2).toList ++
                    items.zipWithIndex.combinations(1)

        List(-1, 1).flatMap(d => {
            combs.map(c => {
                val actuals = c.map(_._1)
                val gens  = actuals.filter(_.isInstanceOf[Generator])
                                   .map(_.asInstanceOf[Generator]).toSet

                val chips = actuals.filter(_.isInstanceOf[Microchip])
                                   .map(_.asInstanceOf[Microchip]).toSet

                step(d, gens, chips)
            })
        }).filter(_ != None).map(_.get)
    }

    def step
        ( change: Int
        , gens: Set[Generator]
        , chips: Set[Microchip])
        : Option[Floors] = {

        val newFloor = elevator + change
        if (newFloor < 1 || newFloor > floors.keys.size) None
        else {
            val newMap: Day11.FloorMap = floors
                .updated(elevator,
                    (floors(elevator)._1 -- gens, floors(elevator)._2 -- chips)
                ).updated(newFloor,
                    (floors(newFloor)._1 ++ gens, floors(newFloor)._2 ++ chips)
                )

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
        k => 31 * k * (floors(k)._1.toList ++ floors(k)._2).map(_.hashCode()).sum
    ).sum

    override def toString(): String = {
        val sb = new StringBuilder()
        for (f <- floors.keys.toList.sorted.reverse) {
            sb.append("F" + f.toString)
            sb.append(if (elevator == f) " E " else " . ")
            sb.append((floors(f)._1.toList ++ floors(f)._2).mkString(" "))
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
