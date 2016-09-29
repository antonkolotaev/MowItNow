package mowitnow

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers with Parser {

    def test[T](res : ParseResult[T], expected : T): Unit = {
        res match {
            case Success(x, _) =>
                x should be (expected)
            case Failure(ex, next) =>
                println(ex)
                println(next)
                assert(false)
        }
    }

    "zero" should "give an integer" in {
        test(parseAll(integer, "0"), 0)
    }

    "3543" should "give an integer" in {
        test(parseAll(integer, "3543"), 3543)
    }

    " 1 2   " should "give dimensions" in  {
        test(parseAll(dimensions, "1 2\n"), Position(1,2))
    }

    "E" should "give East" in {
        test(parseAll(orientation, "E"), Orientation.East)
    }

    "5 7 N " should "give initial position and orientation" in  {
        test(parseAll(mower, "5 7 N\n"), Mower(Position(5,7), Orientation.North))
    }

    "G" should "give Left" in {
        test(parseAll(command, "G"), Left)
    }

    "GADD" should "give command sequence" in  {
        test(parseAll(commands, "GADD"), List(Left, Forward, Right, Right))
    }

    "5 7 N | GADD" should "define a mower" in  {
        test(parseAll(task, "5 7 N\nGADD"), Player.Task(Mower(Position(5,7), Orientation.North), List(Left, Forward, Right, Right)))
    }

    "9 9 | 5 7 N | GADD | 2 5 W | D " should "define a game" in  {
        test(parseAll(all, "9 9\n5 7 N\nGADD\n2 5 W\nD"),
            (Position(9,9),
                List(
                    Player.Task(Mower(Position(5,7), Orientation.North), List(Left, Forward, Right, Right)),
                    Player.Task(Mower(Position(2,5), Orientation.West), List(Right)))))

    }

    "example" should "parse well" in  {
        val s =
            """
              |5 5
              |1 2 N
              |GAGAGAGAA
              |3 3 E
              |AADAADADDA
            """.stripMargin
        test(parseAll(all, s),
            (Position(5,5),
                List(
                    Player.Task(Mower(Position(1,2), Orientation.North), List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                    Player.Task(Mower(Position(3,3), Orientation.East), List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))))
    }
}
