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
                    case Success((upperRight, mowers), _) =>
                        Player.sequential(Lawn fromUpperRight upperRight, mowers) match {
                            case scala.util.Success(positions) =>
                                positions foreach { case (position, orientation) =>
                                    println(s"${position.x} ${position.y} $orientation")
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
