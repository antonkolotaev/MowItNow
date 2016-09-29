package mowitnow

import mowitnow.Player.{DuplicatePosition, OutOfLawn}
import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Failure, Success, Try}

class BaseSpec(player : (Lawn, Seq[Player.Task]) => Try[Seq[Mower]]) extends FlatSpec with Matchers {

    def assignmentSpec = {

        player(
            Lawn fromUpperRight Position(5, 5), List(
                Player.Task(Mower(Position(1, 2), Orientation.North),
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Task(Mower(Position(3, 3), Orientation.East),
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List(Mower(Position(1, 3), Orientation.North), Mower(Position(5, 1), Orientation.East))
            case Failure(ex) =>
                assert(false)
        }

    }

    def outOfLawnSpec = {

        player(
            Lawn fromUpperRight Position(5, 5), List(
                Player.Task(Mower(Position(1, 2), Orientation.North),
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Task(Mower(Position(6, 3), Orientation.East),
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match {
            case Failure(ex) =>
                ex should be(OutOfLawn(Position(6, 3)))
            case Success(result) =>
                assert(false)
        }
    }

    def duplicateSpec = {

        Player.sequential(
            Lawn fromUpperRight Position(5,5), List(
                Player.Task(Mower(Position(1,2), Orientation.North),
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Task(Mower(Position(1,2), Orientation.East),
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Task(Mower(Position(3,3), Orientation.East),
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Failure(ex) =>
                ex should be (DuplicatePosition(Position(1,2)))
            case Success(result) =>
                assert(false)
        }

    }
}
