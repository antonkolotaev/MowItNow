package mowitnow

import scala.util.{Failure, Success}

class ConcurrentSpec extends BaseSpec(Player.concurrent) {

    "concurrent mowers" should "conform to the assignment spec" in assignmentSpec

    it should "report about incorrect mowers" in outOfLawnSpec

    it should "report about duplicate mowers" in duplicateSpec

    it should "bump into unprocessed mowers" in {

        Player.concurrent(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.East,
                    List(Forward, Forward, Left, Forward)),
                Player.Def(Position(4,2), Orientation.West,
                    List(Forward, Forward, Forward, Forward, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List((Position(2,3), Orientation.North), (Position(1,2), Orientation.West))
            case Failure(_) =>
                assert(false)
        }


    }

    it should "bump into processed mowers" in {

        Player.concurrent(
            Position(5,5), List(
                Player.Def(Position(1,2), Orientation.East,
                    List(Forward, Forward, Left, Forward)),
                Player.Def(Position(3,2), Orientation.West,
                    List(Forward, Forward, Forward, Forward, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List((Position(2,3), Orientation.North), (Position(1,2), Orientation.West))
            case Failure(_) =>
                assert(false)
        }


    }
}
