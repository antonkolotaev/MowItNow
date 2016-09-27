package mowitnow

import mowitnow.Player.OutOfLawn
import org.scalatest._

import scala.util.{Failure, Success}

class IndependentSpec extends FlatSpec with Matchers {

    "independent mowers" should "conform to the assignment spec" in {

        Player.independent(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.North,
                    List(Left, Forward, Left, Forward, Left, Forward, Left, Forward, Forward)),
                Player.Def(Position(3,3), Orientation.East,
                    List(Forward, Forward, Right, Forward, Forward, Right, Forward, Right, Right, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List((Position(1,3), Orientation.North), (Position(5,1), Orientation.East))
            case Failure(ex) =>
                assert(false)
        }

    }

    it should "report about incorrect mowers" in {

        Player.independent(
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

}
