package mowitnow

import scala.io.Source

object Sequential extends Parser {

    def main(args : Array[String]) =
    {
        if (args.length != 1)
            println("Only argument should be given: file to process")
        else {
            try {
                val text = Source.fromFile(args(0)).getLines() mkString "\n"

                parseAll(all, text) match {
                    case Success((upperRight, tasks), _) =>
                        Player.sequential(Lawn fromUpperRight upperRight, tasks) match {
                            case scala.util.Success(mowers) =>
                                mowers foreach { mower =>
                                    println(s"${mower.position.x} ${mower.position.y} ${mower.orientation}")
                                }
                            case scala.util.Failure(ex) =>
                                println("Error: " + ex)
                        }
                    case NoSuccess(msg, next) =>
                        println(s"Failed to parse: '$msg' at (${next.pos.line}:${next.pos.column})")
                        println(next.pos.longString)
                }
            } catch {
                case ex: Throwable => println(ex)
            }
        }

    }


}
