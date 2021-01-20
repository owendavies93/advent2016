package advent2016

import annotation.tailrec

package object machine {
    type Machine = Map[String, Int]
}

class Assembunny(init: Map[String, Int]) {
    import advent2016.machine.Machine

    val numeric = """-?\d+""".r

    def run(lines: List[String]): Machine = {
        @tailrec
        def run_(regs: Machine, ptr: Int, comms: List[String]): Machine = {
            if (ptr < 0 || ptr >= comms.size) regs
            else {
                val (regs_, ptr_, comms_) = step(regs, ptr, comms)
                run_(regs_, ptr_, comms_)
            }
        }

        run_(init, 0, lines)
    }

    private def parseLine(line: String): List[String] = line.split(" ").toList

    private def step
        ( regs: Machine
        , ptr: Int
        , comms: List[String])
        : (Machine, Int, List[String]) = {

        val cmd :: args = parseLine(comms(ptr))

        try {
            cmd match {
                case "cpy" => args(0) match {
                    case numeric(_*) =>
                        (regs.updated(args(1), args(0).toInt), ptr + 1, comms)
                    case _ =>
                        (regs.updated(args(1), regs(args(0))), ptr + 1, comms)
                }
                case "inc" =>
                    (regs.updated(args(0), regs(args(0)) + 1), ptr + 1, comms)
                case "dec" =>
                    (regs.updated(args(0), regs(args(0)) - 1), ptr + 1, comms)
                case "jnz" => args(0) match {
                    case numeric(_*) => {
                        if (args(0).toInt != 0) {
                            args(1) match {
                                case numeric(_*) =>
                                    (regs, ptr + args(1).toInt, comms)
                                case _ =>
                                    (regs, ptr + regs(args(1)), comms)
                            }
                        } else {
                            (regs, ptr + 1, comms)
                        }
                    }
                    case _ => {
                        if (regs(args(0)) != 0) {
                            args(1) match {
                                case numeric(_*) =>
                                    (regs, ptr + args(1).toInt, comms)
                                case _ =>
                                    (regs, ptr + regs(args(1)), comms)
                            }
                        } else {
                            (regs, ptr + 1, comms)
                        }
                    }
                }
                case "tgl" => {
                    val index = ptr + regs(args(0))
                    if (index >= 0 && index < comms.size) {
                        val c :: a = parseLine(comms(index))
                        (regs, ptr + 1, comms.updated(index,
                            a.size match {
                                case 1 => (
                                    (if (c == "inc") "dec" else "inc") :: a
                                ).mkString(" ")
                                case 2 => (
                                    (if (c == "jnz") "cpy" else "jnz") :: a
                                ).mkString(" ")
                            }
                        ))
                    } else {
                        (regs, ptr + 1, comms)
                    }
                }
            }
        } catch {
            case _: Throwable => (regs, ptr + 1, comms)
        }
    }
}
