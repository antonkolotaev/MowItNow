package mowitnow

import mowitnow.Player.{DuplicatePosition, OutOfLawn}
import org.scalatest._

import scala.util.{Success, Failure}

class SequentialSpec extends FlatSpec with Matchers {

    "sequential mowers" should "conform to the assignment spec" in {

        Player.sequential(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.North,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Def(Position(3,3), Orientation.East,
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List((Position(1,3), Orientation.North), (Position(5,1), Orientation.East))
            case Failure(_) =>
                assert(false)
        }


    }

    it should "bump into unprocessed mowers" in {

        Player.sequential(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.North,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Def(Position(0,1), Orientation.East,
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List((Position(1,4), Orientation.North), (Position(2,0), Orientation.East))
            case Failure(_) =>
                assert(false)

        }


    }

    it should "bump into processed mowers" in {

        Player.sequential(
            Position(5,5), List(
                Player.Def(Position(0,1), Orientation.East,
                    List()),
                Player.Def(Position(1,2), Orientation.North,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)))
        ) match {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List((Position(0,1), Orientation.East), (Position(1,4), Orientation.North))
            case Failure(_) =>
                assert(false)
        }


    }

    it should "report about incorrect mowers" in {

        Player.sequential(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.North,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Def(Position(6,3), Orientation.East,
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Failure(ex) =>
                ex should be (OutOfLawn(Position(6,3)))
            case Success(result) =>
                assert(false)
        }

    }

    it should "report about duplicate mowers" in {

        Player.sequential(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.North,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Def(Position(1,2), Orientation.East,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Def(Position(3,3), Orientation.East,
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Failure(ex) =>
                ex should be (DuplicatePosition(Position(1,2)))
            case Success(result) =>
                assert(false)
        }

    }
}
