package advent2016

import annotation.tailrec

package object machine {
    type Machine = Map[String, Int]
}

class Assembunny(init: Map[String, Int]) {
    import advent2016.machine.Machine

    val numeric = """\d+""".r

    def parseLine(line: String): List[String] = line.split(" ").toList

    def run(lines: List[String]): Machine = {
        @tailrec
        def run_(regs: Machine, ptr: Int): Machine = {
            if (ptr < 0 || ptr >= lines.size) regs
            else {
                val (regs_, ptr_) = step(regs, ptr, parseLine(lines(ptr)))
                run_(regs_, ptr_)
            }
        }

        run_(init, 0)
    }

    private def step(regs: Machine, ptr: Int, comm: List[String]): (Machine, Int) = {
        val cmd :: args = comm

        cmd match {
            case "cpy" => args(0) match {
                case numeric(_*) => (regs.updated(args(1), args(0).toInt), ptr + 1)
                case _           => (regs.updated(args(1), regs(args(0))), ptr + 1)
            }
            case "inc" => (regs.updated(args(0), regs(args(0)) + 1), ptr + 1)
            case "dec" => (regs.updated(args(0), regs(args(0)) - 1), ptr + 1)
            case "jnz" => args(0) match {
                case numeric(_*) => if (args(0).toInt != 0) (regs, ptr + args(1).toInt)
                                    else (regs, ptr + 1)
                case _           => if (regs(args(0)) != 0) (regs, ptr + args(1).toInt)
                                    else (regs, ptr + 1)
            }
        }
    }
}
