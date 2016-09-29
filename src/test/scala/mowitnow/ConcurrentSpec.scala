package mowitnow

import scala.util.{Failure, Success}

class ConcurrentSpec extends BaseSpec(Player.concurrent) {

    "concurrent mowers" should "conform to the assignment spec" in assignmentSpec

    it should "report about incorrect mowers" in outOfLawnSpec

    it should "report about duplicate mowers" in duplicateSpec

    it should "bump into unprocessed mowers" in {

        Player.concurrent(
            Lawn fromUpperRight Position(5,5), List(
                Player.Def(Mower(Position(1,2), Orientation.East),
                    List(Forward, Forward, Left, Forward)),
                Player.Def(Mower(Position(4,2), Orientation.West),
                    List(Forward, Forward, Forward, Forward, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List(Mower(Position(2,3), Orientation.North), Mower(Position(1,2), Orientation.West))
            case Failure(_) =>
                assert(false)
        }


    }

    it should "bump into processed mowers" in {

        Player.concurrent(
            Lawn fromUpperRight Position(5,5), List(
                Player.Def(Mower(Position(1,2), Orientation.East),
                    List(Forward, Forward, Left, Forward)),
                Player.Def(Mower(Position(3,2), Orientation.West),
                    List(Forward, Forward, Forward, Forward, Forward)))) match
        {
            case Success(result) =>
                result should contain theSameElementsInOrderAs List(Mower(Position(2,3), Orientation.North), Mower(Position(1,2), Orientation.West))
            case Failure(_) =>
                assert(false)
        }


    }
}
