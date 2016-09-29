package mowitnow

import scala.io.Source
import scala.util.Try

object Sequential extends Parser {

    object Modes extends Enumeration {
        type Modes = Value
        val Independent, Sequential, Concurrent = Value
    }
    implicit val modesRead: scopt.Read[Modes.Value] =
        scopt.Read.reads(Modes withName)

    case class Config(input : String, kind : Modes.Modes = Modes.Sequential)
    {
        def player : (Lawn, Seq[Player.Task]) => Try[Seq[Mower]] = kind match {
            case Modes.Independent => Player.independent
            case Modes.Sequential  => Player.sequential
            case Modes.Concurrent  => Player.concurrent
        }
    }

    def main(args : Array[String]) =
    {
        val cliParser = new scopt.OptionParser[Config]("MowItNow")
        {
            head("MowItNow", "0.1")

            arg[String]("<input-file>").required().valueName("<file>").
                action( (x, c) => c.copy(input = x) ).
                text("file with a game definition")

            opt[Modes.Modes]('m', "mode").action( (x, c) =>
                c.copy(kind = x) ).text("game mode (Independent, Sequential or Concurrent)")
        }

        cliParser parse(args, Config("")) match {

            case Some(config) =>
                try {
                    val text = Source.fromFile(config.input).getLines() mkString "\n"

                    parseAll(all, text) match {
                        case Success((upperRight, tasks), _) =>
                            config.player(Lawn fromUpperRight upperRight, tasks) match {
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

            case None =>
                

        }
    }


}
